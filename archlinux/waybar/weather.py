#!/usr/bin/env python3

# https://www.reddit.com/user/Balthazzah/submitted/
# https://gist.github.com/Balthazzahr
# Prerequisite install: 
#   sudo pacman -S python-requests

import requests
import json
import sys
from datetime import datetime
import calendar
import pathlib
import time
import pickle
import html
import time

try:
    import tomllib
except ImportError:
    tomllib = None

LAT = "32.9219971"
LON = "-117.173557"
DISPLAY_NAME = "Sorrento Valley, CA"
CACHE_TIMEOUT = 900

CACHE_FILE = pathlib.Path.home() / ".cache" / "waybar_weather_cache.pkl"
THEME_FILE = pathlib.Path.home() / ".config/omarchy/current/theme/alacritty.toml"

WEATHER_MAP = {
    0: ("", "Clear sky"),
    1: ("", "Mainly clear"),
    2: ("", "Partly cloudy"),
    3: ("", "Overcast"),
    45: ("", "Fog"),
    48: ("", "Depositing rime fog"),
    51: ("", "Light drizzle"),
    53: ("", "Moderate drizzle"),
    55: ("", "Dense drizzle"),
    56: ("", "Light freezing drizzle"),
    57: ("", "Dense freezing drizzle"),
    61: ("", "Slight rain"),
    63: ("", "Moderate rain"),
    65: ("", "Heavy rain"),
    66: ("", "Light freezing rain"),
    67: ("", "Heavy freezing rain"),
    71: ("", "Slight snowfall"),
    73: ("", "Moderate snowfall"),
    75: ("", "Heavy snowfall"),
    77: ("", "Snow grains"),
    80: ("", "Slight rain showers"),
    81: ("", "Moderate rain showers"),
    82: ("", "Violent rain showers"),
    85: ("", "Slight snow showers"),
    86: ("", "Heavy snow showers"),
    95: ("", "Thunderstorm"),
    96: ("", "Thunderstorm with hail"),
    99: ("", "Thunderstorm with heavy hail"),
}

WIND_ARROWS = {
    "N": "↑",
    "NNE": "↗",
    "NE": "↗",
    "ENE": "↗",
    "E": "→",
    "ESE": "↘",
    "SE": "↘",
    "SSE": "↘",
    "S": "↓",
    "SSW": "↙",
    "SW": "↙",
    "WSW": "↙",
    "W": "←",
    "WNW": "↖",
    "NW": "↖",
    "NNW": "↖",
}


def c_to_f(c):
    return (c * 9.0 / 5.0) + 32.0


def kph_to_mph(kph):
    return kph * 0.621371


def get_wind_direction(degrees):
    directions = [
        "N",
        "NNE",
        "NE",
        "ENE",
        "E",
        "ESE",
        "SE",
        "SSE",
        "S",
        "SSW",
        "SW",
        "WSW",
        "W",
        "WNW",
        "NW",
        "NNW",
    ]
    idx = int((degrees + 11.25) / 22.5)
    return directions[idx % 16]


def get_wind_description(speed_mph):
    if speed_mph < 6:
        return ("Calm", "green")
    elif speed_mph < 13:
        return ("Light breeze", "green")
    elif speed_mph < 19:
        return ("Moderate", "yellow")
    elif speed_mph < 25:
        return ("Fresh", "yellow")
    elif speed_mph < 31:
        return ("Strong", "orange")
    elif speed_mph < 39:
        return ("Very strong", "orange")
    elif speed_mph < 47:
        return ("Near gale", "red")
    elif speed_mph < 55:
        return ("Gale", "red")
    elif speed_mph < 64:
        return ("Strong gale", "red")
    return ("DAMAGING WINDS", "red")


def get_uv_info(uv_index):
    if uv_index < 3:
        return ("Low", "green")
    elif uv_index < 6:
        return ("Moderate", "yellow")
    elif uv_index < 8:
        return ("High", "orange")
    elif uv_index < 11:
        return ("Very High", "red")
    return ("Extreme", "purple")


def get_humidity_info(humidity):
    if humidity < 20:
        return (" Extreme Dry  ", "red")
    elif humidity < 30:
        return ("⚡ Very Dry ⚡", "red")
    elif humidity < 40:
        return (" Pleasant ", "orange")
    elif humidity < 50:
        return (" Perfect ", "green")
    elif humidity < 60:
        return (" Little Bit Humid ", "blue")
    elif humidity < 70:
        return (" Getting Sticky ", "blue")
    elif humidity < 80:
        return (" Properly Humid Now ", "blue")
    elif humidity < 90:
        return ("  Tropical Sauna Mode  ", "purple")
    return ("🌊Basically Underwater🌊", "purple")


def calculate_fire_danger(temp_c, humidity, wind_mph):
    if humidity > 70:
        return ("Low-Moderate", "green")
    danger_score = (temp_c * 0.5) + (wind_mph * 1.3) - (humidity * 0.5)
    if danger_score < 12:
        return ("Low-Moderate", "green")
    elif danger_score < 24:
        return ("High", "yellow")
    elif danger_score < 38:
        return ("Very High", "orange")
    elif danger_score < 50:
        return ("Severe", "red")
    elif danger_score < 75:
        return ("Extreme", "red")
    return ("Catastrophic", "purple")


def load_omarchy_colors():
    defaults = {
        "white": "#ffffff",
        "red": "#ff0000",
        "yellow": "#ffff00",
        "green": "#00ff00",
        "blue": "#0000ff",
        "cyan": "#00ffff",
        "purple": "#ca9ee6",
        "bright_black": "#555555",
    }
    if not tomllib or not THEME_FILE.exists():
        return defaults
    try:
        data = tomllib.loads(THEME_FILE.read_text())
        colors = data.get("colors", {})
        normal, bright = colors.get("normal", {}), colors.get("bright", {})
        return {
            "white": normal.get("white", defaults["white"]),
            "red": normal.get("red", defaults["red"]),
            "yellow": normal.get("yellow", defaults["yellow"]),
            "green": normal.get("green", defaults["green"]),
            "blue": normal.get("blue", defaults["blue"]),
            "cyan": normal.get("cyan", defaults["cyan"]),
            "purple": normal.get("magenta", defaults["purple"]),
            "bright_black": bright.get("black", defaults["bright_black"]),
        }
    except Exception:
        return defaults


COLORS = load_omarchy_colors()
COLOR_MAP = {
    "green": COLORS["green"],
    "yellow": COLORS["yellow"],
    "orange": "#ef9f76",
    "red": COLORS["red"],
    "blue": COLORS["blue"],
    "purple": COLORS["purple"],
}


def temp_to_color_f(temp_f):
    TEMP_COLORS_F = [
        (59, COLORS["blue"]),
        (64, COLORS["blue"]),
        (70, COLORS["cyan"]),
        (75, COLORS["cyan"]),
        (81, COLORS["green"]),
        (86, COLORS["yellow"]),
        (90, COLORS["yellow"]),
        (92, COLORS["red"]),
        (212, COLORS["red"]),
    ]
    for t_max, color in TEMP_COLORS_F:
        if temp_f <= t_max:
            return color
    return COLORS["red"]


def get_weather_data():
    CACHE_FILE.parent.mkdir(parents=True, exist_ok=True)
    now = time.time()
    if CACHE_FILE.exists():
        try:
            with open(CACHE_FILE, "rb") as f:
                cached = pickle.load(f)
                if now - cached["timestamp"] < CACHE_TIMEOUT:
                    return cached["data"], now
        except Exception:
            pass

    url = (
        f"https://api.open-meteo.com/v1/forecast?latitude={LAT}&longitude={LON}"
        "&current=temperature_2m,relative_humidity_2m,apparent_temperature,"
        "precipitation,rain,weather_code,wind_speed_10m,wind_direction_10m,uv_index"
        "&hourly=temperature_2m,weather_code,precipitation_probability,precipitation,is_day"
        "&daily=weather_code,temperature_2m_max,temperature_2m_min,precipitation_probability_max,sunrise,sunset"
        "&timezone=auto"
    )
    try:
        r = requests.get(url, timeout=10)
        r.raise_for_status()
        data = r.json()
        with open(CACHE_FILE, "wb") as f:
            pickle.dump({"timestamp": now, "data": data}, f)
        return data, now
    except Exception:
        return None


def main():
    data, now = get_weather_data()
    if not data:
        print(json.dumps({"text": "N/A", "tooltip": "Weather unavailable"}))
        sys.exit(0)

    try:
        curr, hourly, daily = data["current"], data["hourly"], data["daily"]

        temp_c = curr["temperature_2m"]
        feels_like_c = curr["apparent_temperature"]
        humidity = curr["relative_humidity_2m"]

        temp_f = c_to_f(temp_c)
        feels_like_f = c_to_f(feels_like_c)

        wind_kph = curr["wind_speed_10m"]
        wind_mph = kph_to_mph(wind_kph)
        wind_dir = get_wind_direction(curr["wind_direction_10m"])
        uv_index = curr.get("uv_index", 0)
        icon, desc = WEATHER_MAP.get(curr["weather_code"], ("❓", "Unknown"))

        now_iso = datetime.now().isoformat()
        clocks = ["󱑊", "󱐿", "󱑀", "󱑁", "󱑂", "󱑃", "󱑄", "󱑅", "󱑆", "󱑇", "󱑈", "󱑉"]

        lines = [f"<span size='large'> {DISPLAY_NAME} - {icon} {desc}</span>"]
        lines.append(f"<span foreground='{COLORS['cyan']}'><b>{clocks[0]} Last Update: {time.strftime("%H:%M:%S", time.localtime(now))}</b></span>")
        lines.append(
            f" <span foreground='{temp_to_color_f(temp_f)}'><b>{temp_f:.0f}°F</b></span> "
            f"(Feels <span foreground='{temp_to_color_f(feels_like_f)}'>{feels_like_f:.0f}°F</span>)"
        )
        lines.append(f"  {daily['sunrise'][0].split('T')[1]}   {daily['sunset'][0].split('T')[1]}")
        lines.append("")

        w_desc, w_col = get_wind_description(wind_mph)
        h_desc, h_col = get_humidity_info(humidity)
        uv_desc, uv_col = get_uv_info(uv_index)
        f_desc, f_col = calculate_fire_danger(temp_c, humidity, wind_mph)

        lines.append(f" <span foreground='{COLOR_MAP.get(h_col, COLORS['white'])}'>{humidity}%</span> {html.escape(h_desc)}")
        lines.append(
            f"󰖝 <span foreground='{COLOR_MAP.get(w_col, COLORS['white'])}'>"
            f"{WIND_ARROWS.get(wind_dir, '○')} {wind_dir} {wind_mph:.0f} mph ({w_desc})</span>"
        )
        lines.append(f"󰓄 <span foreground='{COLOR_MAP.get(uv_col, COLORS['white'])}'>UV: {uv_index} ({uv_desc})</span>")
        lines.append(f"󱗗 <span foreground='{COLOR_MAP.get(f_col, COLORS['white'])}'>Fire: {f_desc}</span>")
        lines.append("")

        lines.append(f"<span foreground='{COLORS['yellow']}'><b> Today</b></span>")

        for i in range(24):
            if hourly["time"][i] >= now_iso[:13]:
                dt_h = datetime.fromisoformat(hourly["time"][i])
                h_prob = hourly["precipitation_probability"][i]
                h_temp_f = c_to_f(hourly["temperature_2m"][i])
                h_icon, h_desc = WEATHER_MAP.get(hourly["weather_code"][i], (" ", "Unknown"))

                rain_color = COLORS["blue"] if h_prob > 0 else COLORS["bright_black"]
                clock_icon = clocks[dt_h.hour % 12]
                time_str = dt_h.strftime(f"{clock_icon} %I:%M %p")
                label_col = f"{time_str:<12}"
                rain_col = f"<span foreground='{rain_color}'> {h_prob:>2}%</span>"
                temp_col = f"<span foreground='{temp_to_color_f(h_temp_f)}'> {h_temp_f:>5.1f}°F</span>"

                lines.append(f"<span font_family='monospace'>{label_col}  {rain_col}  {temp_col}  {h_icon:<2} {h_desc}</span>")

        lines.append(f"\n<span foreground='{COLORS['green']}'><b> Tomorrow</b></span>")
        time_data = {7: ("󰖜", "Morning"), 12: ("󰖙", "Midday"), 17: ("󰖚", "Afternoon"), 21: ("󰖔", "Evening")}

        for i in range(24, 48):
            dt = datetime.fromisoformat(hourly["time"][i])
            if dt.hour in time_data:
                glyph, label_text = time_data[dt.hour]
                t_prob = hourly["precipitation_probability"][i]
                t_temp_f = c_to_f(hourly["temperature_2m"][i])
                t_icon, t_desc = WEATHER_MAP.get(hourly["weather_code"][i], (" ", "Unknown"))
                rain_color = COLORS["blue"] if t_prob > 0 else COLORS["bright_black"]
                label_col = f"{glyph} {label_text:<10}"
                rain_col = f"<span foreground='{rain_color}'> {t_prob:>2}%</span>"
                temp_col = f"<span foreground='{temp_to_color_f(t_temp_f)}'> {t_temp_f:>5.1f}°F</span>"
                lines.append(f"<span font_family='monospace'>{label_col}  {rain_col}  {temp_col}  {t_icon:<2} {t_desc}</span>")

        lines.append(f"\n<span foreground='{COLORS['blue']}'><b> Extended Forecast</b></span>")

        for i in range(1, min(7, len(daily["time"]))):
            dt = datetime.fromisoformat(daily["time"][i])
            d_min_f = c_to_f(daily["temperature_2m_min"][i])
            d_max_f = c_to_f(daily["temperature_2m_max"][i])
            r_prob = daily.get("precipitation_probability_max", [0] * 7)[i]
            start_idx, end_idx = i * 24 + 9, i * 24 + 18
            daytime_codes = hourly["weather_code"][start_idx:end_idx]
            d_code = max(set(daytime_codes), key=daytime_codes.count) if daytime_codes else daily["weather_code"][i]
            d_icon, d_text = WEATHER_MAP.get(d_code, (" ", "Unknown"))
            rain_color = COLORS["blue"] if r_prob > 0 else COLORS["bright_black"]
            day_num = dt.strftime("%d")
            calendar_tab = f"<span background='{COLORS['white']}' foreground='#1e1e2e'>{day_num}</span>"
            day_name_str = f"{calendar.day_name[dt.weekday()]:<9}"
            day_label = f"{calendar_tab} {day_name_str}"
            t_min_s = f"<span foreground='{temp_to_color_f(d_min_f)}'>{d_min_f:>2.0f}</span>"
            t_max_s = f"<span foreground='{temp_to_color_f(d_max_f)}'>{d_max_f:>2.0f}</span>"
            temp_col = f" {t_min_s}  {t_max_s}"
            line = (
                f"<span font_family='monospace'>{day_label}  "
                f"<span foreground='{rain_color}'> {r_prob:>2}%</span>  "
                f"{temp_col}  {d_icon:<2} {d_text}</span>"
            )
            lines.append(line)
            if i < 6:
                lines.append("<span size='3000'> </span>")

        print(
            json.dumps(
                {
                    "text": f" | {icon} <span foreground='{temp_to_color_f(temp_f)}'>{temp_f:.0f}°F</span> ",
                    "tooltip": "\n".join(lines),
                    "markup": "pango",
                    "class": "weather",
                },
                ensure_ascii=False,
            )
        )

    except Exception as e:
        print(json.dumps({"text": "Error", "tooltip": str(e)}))


if __name__ == "__main__":
    main()
