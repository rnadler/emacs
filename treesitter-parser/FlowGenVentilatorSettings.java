package easycare.model;

import easycare.model.json.JsonPathProperty;
import easycare.util.SettingsAcronymUtil;
import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import resmed.cal.common.dto.SettingsCircuit;
import resmed.cal.common.dto.SettingsCycleSensitivity;
import resmed.cal.common.dto.SettingsDurationOption;
import resmed.cal.common.dto.SettingsFlowShape;
import resmed.cal.common.dto.SettingsInterface;
import resmed.cal.common.dto.SettingsManualBreathEnable;
import resmed.cal.common.dto.SettingsPatientType;
import resmed.cal.common.dto.SettingsRiseEnable;
import resmed.cal.common.dto.SettingsSighAlert;
import resmed.cal.common.dto.SettingsSighEnable;
import resmed.cal.common.dto.SettingsTriggerSensitivity;
import resmed.cal.common.dto.SettingsTriggerType;
import resmed.cal.common.dto.SettingsTriggerTypeMouthpiece;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;
import java.math.BigDecimal;
import java.util.Map;

import static javax.persistence.EnumType.STRING;

@EqualsAndHashCode(exclude = "id")
@Entity(name = "FlowGenVentilatorSettings")
@Table(name = "flow_gen_ventilator_settings")
@ToString(exclude = {"id"})
@NoArgsConstructor(access = AccessLevel.PUBLIC)
public class FlowGenVentilatorSettings {
    @GeneratedValue
    @Id
    @Column(name = "flow_gen_ventilator_settings_id")
    @Getter
    @Setter
    private Long id;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.['Val.Circuit']")
    @AcronymMap(value = "CT2", clazz = SettingsCircuit.class, method = "fromSettingsCircuit")
    @Getter
    @Setter
    private Circuit circuit;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.['Val.Interface']")
    @AcronymMap(value = "CT3", clazz = SettingsInterface.class, method = "fromSettingsInterface")
    @Getter
    @Setter
    private PatientInterface patientInterface;

    @JsonPathProperty(path = "$.['Val.PressAssist']")
    @AcronymMap(value = "PAS",  useConstructor = true, clazz = BigDecimal.class)
    @Getter
    @Setter
    private BigDecimal pressureAssist;

    @JsonPathProperty(path = "$.['Val.SafetyTidVolume']")
    @AcronymMap(value = "TVS", useConstructor = true, clazz = BigDecimal.class)
    @Getter
    @Setter
    private BigDecimal safetyTidalVolume;

    @JsonPathProperty(path = "$.['Val.TidalVolume']")
    @AcronymMap(value = "TVL", useConstructor = true, clazz = BigDecimal.class)
    @Getter
    @Setter
    private BigDecimal tidalVolume;

    @JsonPathProperty(path = "$.['Val.Ti'," +
            "'Timed.Set.Ti', 'Timed.Val.Ti'," +
            "'PAC.Set.Ti', 'PAC.Val.Ti'" +
            "]")
    @AcronymMaps({
            @AcronymMap(value = "IPT", useConstructor = true, clazz = BigDecimal.class),
            @AcronymMap(value = "ITT", useConstructor = true, clazz = BigDecimal.class)})
    @Getter
    @Setter
    @Column(name = "inspiratory_time_sec")
    private BigDecimal inspiratoryTime;

    @JsonPathProperty(path = "$.[" +
            "'Val.TiMin'," +
            "'Spont.Set.TiMin', 'Spont.Val.TiMin'," +
            "'ST.Set.TiMin', 'ST.Val.TiMin'," +
            "'iVAPS.Set.TiMin', 'iVAPS.Val.TiMin'," +
            "'VAuto.Set.TiMin', 'VAuto.Val.TiMin'" +
            "]")
    @AcronymMap(value = "ITN", useConstructor = true, clazz = BigDecimal.class)
    @Getter
    @Setter
    @Column(name = "min_inspiratory_time_sec")
    private BigDecimal minInspiratoryTime;

    @JsonPathProperty(path = "$.[" +
            "'Val.TiMax'," +
            "'Spont.Set.TiMax', 'Spont.Val.TiMax'," +
            "'ST.Set.TiMax', 'ST.Val.TiMax'," +
            "'iVAPS.Set.TiMax', 'iVAPS.Val.TiMax'," +
            "'VAuto.Set.TiMax', 'VAuto.Val.TiMax'" +
            "]")
    @AcronymMap(value = "ITX", useConstructor = true, clazz = BigDecimal.class)
    @Getter
    @Setter
    @Column(name = "max_inspiratory_time_sec")
    private BigDecimal maxInspiratoryTime;

    @JsonPathProperty(path = "$.[" +
            "'Val.RiseTime'," +
            "'Spont.Set.RiseTime', 'Spont.Val.RiseTime'," +
            "'ST.Set.RiseTime', 'ST.Val.RiseTime'," +
            "'Timed.Set.RiseTime', 'Timed.Val.RiseTime'," +
            "'PAC.Set.RiseTime', 'PAC.Val.RiseTime'," +
            "'iVAPS.Set.RiseTime', 'iVAPS.Val.RiseTime'" +
            "]")
    @AcronymMap(value = "RST", useConstructor = true, clazz = BigDecimal.class)
    @Getter
    @Setter
    @Column(name = "rise_time_sec")
    private BigDecimal riseTime;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.[" +
            "'Val.Trigger'," +
            "'Spont.Set.Trigger', 'Spont.Val.Trigger'," +
            "'ST.Set.Trigger', 'ST.Val.Trigger'," +
            "'PAC.Set.Trigger', 'PAC.Val.Trigger'," +
            "'iVAPS.Set.Trigger', 'iVAPS.Val.Trigger'," +
            "'VAuto.Set.Trigger', 'VAuto.Val.Trigger'" +
            "]")
    @AcronymMap(value = "VTS", clazz = SettingsTriggerSensitivity.class, method = "fromTriggerSensitivity")
    @Getter
    @Setter
    @Column(name = "trigger_absolute_sensitivity_type")
    private TriggerSensitivity triggerSensitivity;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.[" +
            "'Val.Cycle'," +
            "'Spont.Set.Cycle', 'Spont.Val.Cycle'," +
            "'ST.Set.Cycle', 'ST.Val.Cycle'," +
            "'iVAPS.Set.Cycle', 'iVAPS.Val.Cycle'," +
            "'VAuto.Set.Cycle', 'VAuto.Val.Cycle'" +
            "]")
    @AcronymMap(value = "VCS", clazz = SettingsCycleSensitivity.class, method = "fromCycleSensitivity")
    @Getter
    @Setter
    @Column(name = "cycle_sensitivity_type")
    private CycleSensitivity cycleSensitivity;

    @JsonPathProperty(path = "$.['Val.TrigThresh']")
    @AcronymMap(value = "TGI", useConstructor = true, clazz = BigDecimal.class)
    @Getter
    @Setter
    @Column(name = "trigger_relative_threshold_lpm")
    private BigDecimal triggerThreshold;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.['Val.TriggerType']")
    @AcronymMap(value = "TRT", clazz = SettingsTriggerType.class, method = "fromTriggerType")
    @Getter
    @Setter
    private TriggerType triggerType;

    @JsonPathProperty(path = "$.['Val.CycleThresh']")
    @AcronymMap(value = "TGE", useConstructor = true, clazz = BigDecimal.class)
    @Getter
    @Setter
    @Column(name = "cycle_threshold_pct")
    private BigDecimal cycleThreshold;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.['Val.PatientType']")
    @AcronymMap(value = "CT1", clazz = SettingsPatientType.class, method = "fromPatientType")
    @Getter
    @Setter
    private PatientType patientType;

    @JsonPathProperty(path = "$.['Val.HeightCM'," +
            "'iVAPS.Set.HeightCM', 'iVAPS.Val.HeightCM'" +
            "]")
    @AcronymMap(value = "PHT", useConstructor = true, clazz = BigDecimal.class)
    @Getter
    @Setter
    @Column(name = "height_cm")
    private BigDecimal height;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.['Val.DurationOption']")
    @AcronymMap(value = "IDO", clazz = SettingsDurationOption.class, method = "fromVolumeBreathOptions")
    @Getter
    @Setter
    @Column(name = "volume_breath_option_type")
    private VolumeBreathOptions volumeBreathOptions;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.['Val.FlowShape']")
    @AcronymMap(value = "FSP", clazz = SettingsFlowShape.class, method = "fromFlowShape")
    @Getter
    @Setter
    private FlowShape flowShape;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.['Val.ManualBreathEnable']")
    @AcronymMap(value = "MBR", clazz = SettingsManualBreathEnable.class, method = "fromManualBreath")
    @Getter
    @Setter
    @Column(name = "manual_breath_switch")
    private ManualBreath manualBreath;

    @JsonPathProperty(path = "$.['Val.ManualMag']")
    @AcronymMap(value = "MMA", useConstructor = true, clazz = BigDecimal.class)
    @Getter
    @Setter
    @Column(name = "manual_magnitude_pct")
    private BigDecimal manualMagnitude;

    @JsonPathProperty(path = "$.['Val.PeakFlow']")
    @AcronymMap(value = "PFS", useConstructor = true, clazz = BigDecimal.class)
    @Getter
    @Setter
    @Column(name = "peak_flow_lpm")
    private BigDecimal peakFlow;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.['Val.Sigh.Alert']")
    @AcronymMap(value = "SIA", clazz = SettingsSighAlert.class, method = "fromSighAlert")
    @Getter
    @Setter
    @Column(name = "sigh_alert_switch")
    private SighAlert sighAlert;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.['Val.Sigh.Enable']")
    @AcronymMap(value = "SIG", clazz = SettingsSighEnable.class, method = "fromSighEnable")
    @Getter
    @Setter
    @Column(name = "sigh_enable_switch")
    private SighEnable sighEnable;

    @JsonPathProperty(path = "$.['Val.Sigh.Interval']")
    @AcronymMap(value = "SII", useConstructor = true, clazz = BigDecimal.class)
    @Getter
    @Setter
    @Column(name = "sigh_interval_min")
    private BigDecimal sighInterval;

    @JsonPathProperty(path = "$.['Val.Sigh.Mag']")
    @AcronymMap(value = "SIM", useConstructor = true, clazz = BigDecimal.class)
    @Getter
    @Setter
    @Column(name = "sigh_magnitude_pct")
    private BigDecimal sighMagnitude;

    @JsonPathProperty(path = "$.['Val.StartEPAP']")
    @AcronymMap(value = "EPS", useConstructor = true, clazz = BigDecimal.class)
    @Getter
    @Setter
    @Column(name = "start_epap_cmh2o")
    private BigDecimal startEpap;

    @JsonPathProperty(path = "$.['Val.FallTime']")
    @AcronymMap(value = "WFT", useConstructor = true, clazz = BigDecimal.class)
    @Getter
    @Setter
    @Column(name = "fall_time_sec")
    private BigDecimal fallTime;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.['Val.TriggerTypeMouthpiece']")
    @AcronymMap(value = "TGM", clazz = SettingsTriggerTypeMouthpiece.class, method = "fromSettingsTriggerTypeMouthpiece")
    @Getter
    @Setter
    private TriggerTypeMouthpiece triggerTypeMouthpiece;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.[" +
            "'Spont.Set.RiseEnable', 'Spont.Val.RiseEnable'," +
            "'ST.Set.RiseEnable', 'ST.Val.RiseEnable'," +
            "'PAC.Set.RiseEnable', 'PAC.Val.RiseEnable'," +
            "'Timed.Set.RiseEnable', 'Timed.Val.RiseEnable'," +
            "'iVAPS.Set.RiseEnable', 'iVAPS.Val.RiseEnable'" +
            "]")
    @AcronymMap(value = "RSC", clazz = SettingsRiseEnable.class, method = "fromSettingsRiseEnable")
    @Getter
    @Setter
    @Column(name = "rise_enable")
    private RiseEnable riseEnable;

    @Transient
    private static Map<String, SettingsAcronymMapper> acronymMap = null;

    private static Map<String, SettingsAcronymMapper> getAcronymMap() {
        if (acronymMap == null) {
            acronymMap = SettingsAcronymUtil.getSettingsAcronymMapper(FlowGenVentilatorSettings.class);
        }
        return acronymMap;
    }

    public static SettingsAcronymMapper getAcronymMapper(String acronym) {
        return getAcronymMap().get(acronym);
    }

}
