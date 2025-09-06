#!/usr/bin/env python3
import os, sys, time, json
from collections import deque

def get_len():
    v = os.getenv("SPARK_LEN")
    if v: return int(v)
    if len(sys.argv) >= 3: return int(sys.argv[2])
    return 10
SPARK_LEN = get_len()
SLEEP_SECONDS = 2
BARS = "▁▂▃▄▅▆▇█"
def spark(vals):
    if not vals: return ""
    hi = max(vals) or 1.0
    n = len(BARS) - 1
    return "".join(BARS[min(n, max(0, int(round((v/hi) * n))))] for v in vals)

# Optional (fixed 0–100% scale, good for CPU/Mem):

def spark_pct(vals):
    n = len(BARS) - 1
    return "".join(BARS[min(n, max(0, int(round((v/100.0) * n))))] for v in vals)

def cpu_loop():
    def read():
        with open("/proc/stat") as f:
            p = f.readline().split()[1:]
        p = list(map(int, p[:10] + [0]*10))[:10]
        user,nice,system,idle,iowait,irq,softirq,steal,guest,gnice = p
        idle_all = idle + iowait
        non_idle = user + nice + system + irq + softirq + steal
        total = idle_all + non_idle
        return total, idle_all
    prev_t, prev_i = read()
    hist = deque(maxlen=SPARK_LEN)
    while True:
        time.sleep(SLEEP_SECONDS)
        t, i = read()
        dt, di = t - prev_t, i - prev_i
        prev_t, prev_i = t, i
        usage = 0.0 if dt <= 0 else (dt - di) / dt
        hist.append(usage*100.0)
        out = {
            "text": f"{spark_pct(hist)} {int(usage*100)}% 󰍛 ",
            "tooltip": f"CPU {usage*100:.1f}%",
            "class": "cpu-graph",
            "percentage": int(usage*100)
        }
        print(json.dumps(out), flush=True)

def mem_loop():
    hist = deque(maxlen=SPARK_LEN)
    while True:
        with open("/proc/meminfo") as f:
            m = {k.rstrip(':'): int(v.split()[0]) for k,v in (line.split(None,1) for line in f)}
        total = m.get("MemTotal", 1)
        avail = m.get("MemAvailable", 0)
        used_pct = 100.0 * (1 - avail/total)
        hist.append(used_pct)
        out = {
            "text": f"{spark(hist)} {int(used_pct)}%",
            "tooltip": f"Mem {used_pct:.1f}%",
            "class": "mem-graph",
            "percentage": int(used_pct)
        }
        print(json.dumps(out), flush=True)
        time.sleep(SLEEP_SECONDS)

def pick_iface():
    for env in ("WAYBAR_NET_IFACE","NET_IFACE"):
        if os.getenv(env): return os.getenv(env)
    for iface in os.listdir("/sys/class/net"):
        if iface == "lo": continue
        try:
            with open(f"/sys/class/net/{iface}/operstate") as f:
                if f.read().strip() == "up":
                    return iface
        except: pass
    return "eth0"

def net_loop():
    iface = pick_iface()
    def read_bytes():
        with open(f"/sys/class/net/{iface}/statistics/rx_bytes") as f: rx = int(f.read())
        with open(f"/sys/class/net/{iface}/statistics/tx_bytes") as f: tx = int(f.read())
        return rx, tx
    rx0, tx0 = read_bytes()
    h_down = deque(maxlen=SPARK_LEN)
    h_up = deque(maxlen=SPARK_LEN)
    while True:
        time.sleep(SLEEP_SECONDS)
        rx1, tx1 = read_bytes()
        down = max(0, rx1 - rx0) * 8  # bits/s (1s interval)
        up   = max(0, tx1 - tx0) * 8
        rx0, tx0 = rx1, tx1
        h_down.append(down)
        h_up.append(up)
        text = f"↓{spark(h_down)} ↑{spark(h_up)} 󰀂 "
        def fmt(bps):
            units = ["b/s","Kb/s","Mb/s","Gb/s"]
            val = float(bps)
            i = 0
            while val >= 1000 and i < len(units)-1:
                val /= 1000.0; i += 1
            return f"{val:.2f} {units[i]}"
        out = {
            "text": text,
            "tooltip": f"{iface}\n↓ {fmt(down)}\n↑ {fmt(up)}",
            "class": "net-graph"
        }
        print(json.dumps(out), flush=True)

if __name__ == "__main__":
    mode = (sys.argv[1] if len(sys.argv) > 1 else "cpu").lower()
    {"cpu": cpu_loop, "mem": mem_loop, "net": net_loop}.get(mode, cpu_loop)()
