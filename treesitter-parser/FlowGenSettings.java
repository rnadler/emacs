package easycare.model;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.google.common.base.Objects;
import easycare.model.json.JsonPathProperty;
import easycare.util.SettingsAcronymUtil;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import org.hibernate.annotations.Type;
import resmed.cal.common.dto.SettingsAutoEpap;
import resmed.cal.common.dto.SettingsComfortMode;
import resmed.cal.common.dto.SettingsDesatRule;
import resmed.cal.common.dto.SettingsEPRLevel;
import resmed.cal.common.dto.SettingsEPRMode;
import resmed.cal.common.dto.SettingsEasyBreathe;
import resmed.cal.common.dto.SettingsIBREnable;
import resmed.cal.common.dto.SettingsMask;
import resmed.cal.common.dto.SettingsMode;
import resmed.cal.common.dto.SettingsRampDownEnable;
import resmed.cal.common.dto.SettingsRampEnable;
import resmed.cal.common.dto.SettingsRespRateEnable;
import resmed.cal.common.metadata.TherapyDeviceSettingEnum;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.OneToOne;
import javax.persistence.PostLoad;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.validation.constraints.NotNull;
import java.math.BigDecimal;
import java.util.Map;

import static javax.persistence.EnumType.STRING;

@Entity(name = "FlowGenSettings")
@Table(name = "flow_gen_settings")
@NamedQueries({
        @NamedQuery(name = "findSettingIdsByPatient", query = "SELECT fgsd.flowGenSettings.id FROM FlowGenSummaryData fgsd " +
                "WHERE fgsd.patient = :patient AND fgsd.flowGenSettings != null"),
        @NamedQuery(name = "deleteSettingsByIds", query = "DELETE FlowGenSettings fgs WHERE fgs.id IN " +
                " (:listofIds) ")})
@ToString(exclude = {"id", "acronymMap", "flowGenVentilatorSettings"})
@NoArgsConstructor(access = AccessLevel.PROTECTED)
public class FlowGenSettings {
    private static final String PLACEHOLDER_VAL = "Y";

    @Id
    @GeneratedValue
    @Getter
    @Setter
    private Long id;

    @Column
    @Getter
    @Setter
    private String isPlaceholder;

    @NotNull
    @Enumerated(STRING)
    @AcronymMaps({
            @AcronymMap(value = "MOP", clazz = SettingsMode.class, method = "fromSettingsMode"),
            @AcronymMap(value = "MOQ", clazz = SettingsMode.class, method = "fromSettingsMode")})
    @Getter
    @Setter
    private TherapyMode therapyMode;

    @Enumerated(STRING)
    @AcronymMap(value = "EPT", clazz = SettingsEPRMode.class, method = "fromCSettingsEPRMode")
    @Getter
    @Setter
    private EprMode eprMode;

    @Enumerated(STRING)
    @AcronymMaps({
            @AcronymMap(value = "EPA", clazz = SettingsEPRLevel.class, method = "fromSettingsEprLevel"),
            @AcronymMap(value = "EPR", clazz = String.class, method = "fromCalOrEcoString")})
    @Getter
    private EprLevel eprLevel;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.[" +
            "'CPAP.Set.Ramp.RampEnable', 'CPAP.Val.Ramp.RampEnable', " +
            "'AutoSet.Set.Ramp.RampEnable', 'AutoSet.Val.Ramp.RampEnable', " +
            "'VAuto.Set.Ramp.RampEnable', 'VAuto.Val.Ramp.RampEnable', " +
            "'Spont.Set.Ramp.RampEnable', 'Spont.Val.Ramp.RampEnable', " +
            "'ST.Set.Ramp.RampEnable', 'ST.Val.Ramp.RampEnable', " +
            "'Timed.Set.Ramp.RampEnable', 'Timed.Val.Ramp.RampEnable', " +
            "'ASV.Set.Ramp.RampEnable', 'ASV.Val.Ramp.RampEnable', " +
            "'ASVAuto.Set.Ramp.RampEnable', 'ASVAuto.Val.Ramp.RampEnable', " +
            "'HerAuto.Set.Ramp.RampEnable', 'HerAuto.Val.Ramp.RampEnable', " +
            "'APAP.Set.Ramp.RampEnable', 'APAP.Val.Ramp.RampEnable', " +
            "'PAC.Set.Ramp.RampEnable', 'PAC.Val.Ramp.RampEnable', " +
            "'iVAPS.Set.Ramp.RampEnable', 'iVAPS.Val.Ramp.RampEnable']")
    @AcronymMap(value = "RMA", clazz = SettingsRampEnable.class, method = "fromSettingsRampEnable")
    @Getter
    @Setter
    private RampEnable rampEnable;

    @JsonPathProperty(path = "$.[" +
            "'CPAP.Set.Ramp.RampTime', 'CPAP.Val.Ramp.RampTime', " +
            "'AutoSet.Set.Ramp.RampTime', 'AutoSet.Val.Ramp.RampTime', " +
            "'VAuto.Set.Ramp.RampTime', 'VAuto.Val.Ramp.RampTime', " +
            "'Spont.Set.Ramp.RampTime', 'Spont.Val.Ramp.RampTime', " +
            "'ST.Set.Ramp.RampTime', 'ST.Val.Ramp.RampTime', " +
            "'Timed.Set.Ramp.RampTime', 'Timed.Val.Ramp.RampTime', " +
            "'ASV.Set.Ramp.RampTime', 'ASV.Val.Ramp.RampTime', " +
            "'ASVAuto.Set.Ramp.RampTime', 'ASVAuto.Val.Ramp.RampTime', " +
            "'HerAuto.Set.Ramp.RampTime', 'HerAuto.Val.Ramp.RampTime', " +
            "'APAP.Set.Ramp.RampTime', 'APAP.Val.Ramp.RampTime', " +
            "'PAC.Set.Ramp.RampTime', 'PAC.Val.Ramp.RampTime', " +
            "'iVAPS.Set.Ramp.RampTime', 'iVAPS.Val.Ramp.RampTime']")
    @AcronymMap(value = "RMT", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Integer rampTime;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.[" +
            "'Spont.Set.Ramp.RampDownEnable', 'Spont.Val.Ramp.RampDownEnable', " +
            "'ST.Set.Ramp.RampDownEnable', 'ST.Val.Ramp.RampDownEnable', " +
            "'Timed.Set.Ramp.RampDownEnable', 'Timed.Val.Ramp.RampDownEnable', " +
            "'PAC.Set.Ramp.RampDownEnable', 'PAC.Val.Ramp.RampDownEnable', " +
            "'iVAPS.Set.Ramp.RampDownEnable', 'iVAPS.Val.Ramp.RampDownEnable']")
    @AcronymMap(value = "RDA", clazz = SettingsRampDownEnable.class, method = "fromSettingsRampDownEnable")
    @Getter
    @Transient
    private RampDownEnable rampDownEnableView;

    @Type(type = "yes_no")
    private Boolean rampDownEnable;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.[" +
            "'CPAP.Set.DesatRule', 'CPAP.Val.DesatRule', " +
            "'Spont.Set.DesatRule', 'Spont.Val.DesatRule', " +
            "'ST.Set.DesatRule', 'ST.Val.DesatRule', " +
            "'Timed.Set.DesatRule', 'Timed.Val.DesatRule', " +
            "'iVAPS.Set.DesatRule', 'iVAPS.Val.DesatRule', " +
            "'ASV.Set.DesatRule', 'ASV.Val.DesatRule', " +
            "'ASVAuto.Set.DesatRule', 'ASVAuto.Val.DesatRule', " +
            "'PAC.Set.DesatRule', 'PAC.Val.DesatRule']")
    @AcronymMap(value = "ODT", clazz = SettingsDesatRule.class, method = "fromSettingsDesatRule")
    @Getter
    @Setter
    private DesatRuleThreshold desatRuleThreshold;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.['AutoSet.Set.Comfort', 'AutoSet.Val.Comfort']")
    @AcronymMap(value = "AFC", clazz = SettingsComfortMode.class, method = "fromSettingsComfortMode")
    @Getter
    @Setter
    private ComfortMode comfortMode;

    @JsonPathProperty(path = "$.[" +
            "'CPAP.Set.StartPress',  'CPAP.Val.StartPress', " +
            "'AutoSet.Set.StartPress','AutoSet.Val.StartPress', " +
            "'HerAuto.Set.StartPress','HerAuto.Val.StartPress', " +
            "'APAP.Set.StartPress','APAP.Val.StartPress', " +
            "'Val.StartPress']")
    @AcronymMaps({
            @AcronymMap(value = "STP", useConstructor = true, clazz = BigDecimal.class),
            @AcronymMap(value = "STU", useConstructor = true, clazz = BigDecimal.class)})
    @Getter
    @Setter
    private BigDecimal startPressure;

    @JsonPathProperty(path = "$.[ " +
            "'Spont.Set.StartEPAP', 'Spont.Val.StartEPAP', " +
            "'ST.Set.StartEPAP', 'ST.Val.StartEPAP', " +
            "'Timed.Set.StartEPAP', 'Timed.Val.StartEPAP', " +
            "'PAC.Set.StartEPAP', 'PAC.Val.StartEPAP', " +
            "'iVAPS.Set.StartEPAP', 'iVAPS.Val.StartEPAP', " +
            "'VAuto.Set.StartEPAP', 'VAuto.Val.StartEPAP', " +
            "'ASV.Set.StartEPAP', 'ASV.Val.StartEPAP', " +
            "'ASVAuto.Set.StartEPAP', 'ASVAuto.Val.StartEPAP']")
    @AcronymMaps({
            @AcronymMap(value = "EPS", useConstructor = true, clazz = BigDecimal.class),
            @AcronymMap(value = "IVS", useConstructor = true, clazz = BigDecimal.class),
            @AcronymMap(value = "STV", useConstructor = true, clazz = BigDecimal.class),
            @AcronymMap(value = "STE", useConstructor = true, clazz = BigDecimal.class),
            @AcronymMap(value = "EAS", useConstructor = true, clazz = BigDecimal.class)})
    @Getter
    @Setter
    private BigDecimal startEpap;

    @JsonPathProperty(path = "$.['CPAP.Set.Press', 'CPAP.Val.Press', 'Val.Press']")
    @AcronymMap(value = "IPC", clazz = String.class, method = "valueOf")
    private Double setPressure;

    @JsonPathProperty(path = "$.['AutoSet.Set.MinPress','HerAuto.Set.MinPress','AutoSet.Val.MinPress','HerAuto.Val.MinPress']")
    @AcronymMap(value = "MPI", clazz = String.class, method = "valueOf")
    private Double minimumPressure;

    @JsonPathProperty(path = "$.['AutoSet.Set.MaxPress','HerAuto.Set.MaxPress', 'AutoSet.Val.MaxPress','HerAuto.Val.MaxPress']")
    @AcronymMap(value = "MPA", clazz = String.class, method = "valueOf")
    private Double maximumPressure;

    @JsonPathProperty(path = "$.[" +
            "'Spont.Set.EPAP', 'ST.Set.EPAP','Timed.Set.EPAP', 'PAC.Set.EPAP'," +
            "'Spont.Val.EPAP', 'ST.Val.EPAP','Timed.Val.EPAP', 'PAC.Val.EPAP'," +
            "'Val.EPAP']")
    @AcronymMap(value = "EPP", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Double expirationPressure;

    @JsonPathProperty(path = "$.[" +
            "'Spont.Set.IPAP', 'ST.Set.IPAP','Timed.Set.IPAP', 'PAC.Set.IPAP', " +
            "'Spont.Val.IPAP', 'ST.Val.IPAP','Timed.Val.IPAP', 'PAC.Val.IPAP', " +
            "'Val.IPAP']")
    @AcronymMap(value = "IPP", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Double inspirationPressure;

    @JsonPathProperty(path = "$.['ASV.Set.EPAP', 'ASV.Val.EPAP']")
    @AcronymMap(value = "EEP", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Double epapasv;

    @JsonPathProperty(path = "$.['Spont.Set.EasyBreathe', 'Spont.Val.EasyBreathe']")
    @AcronymMap(value = "EBE", clazz = SettingsEasyBreathe.class, method = "getValue", useClazz = true)
    @Getter
    @Setter
    private String easyBreathe;

    @JsonPathProperty(path = "$.['VAuto.Set.MaxIPAP', 'VAuto.Val.MaxIPAP', 'Val.MaxIPAP']")
    @AcronymMaps({
            @AcronymMap(value = "MXI", clazz = String.class, method = "valueOf"),
            @AcronymMap(value = "MIP", clazz = String.class, method = "valueOf")})
    @Getter
    @Setter
    private Double maxIpap;

    @JsonPathProperty(path = "$.['VAuto.Set.MinEPAP', 'VAuto.Val.MinEPAP']")
    @AcronymMap(value = "MNE", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Double minEpap;

    @JsonPathProperty(path = "$.['VAuto.Set.PS', 'VAuto.Val.PS', 'Val.PressSupport']")
    @AcronymMap(value = "SPT", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Double pressureSupport;

    @JsonPathProperty(path = "$.['ASV.Set.MaxPS', 'ASV.Val.MaxPS', 'Val.MaxPS']")
    @AcronymMap(value = "MXS", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Double maxPs;

    @JsonPathProperty(path = "$.['ASV.Set.MinPS', 'ASV.Val.MinPS']")
    @AcronymMap(value = "MNS", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Double minPs;

    @JsonPathProperty(path = "$.[" +
            "'ST.Set.BackupRate','Timed.Set.BackupRate', 'PAC.Set.BackupRate'," +
            "'ST.Val.BackupRate','Timed.Val.BackupRate', 'PAC.Val.BackupRate']")
    @AcronymMap(value = "BRR", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Double backupRate;

    @JsonPathProperty(path = "$.[" +
            "'ST.Set.RespRate','Timed.Set.RespRate', 'PAC.Set.RespRate','Spont.Set.RespRate'," +
            "'ST.Val.RespRate','Timed.Val.RespRate', 'PAC.Val.RespRate','Spont.Val.RespRate'," +
            "'Val.RespRate']")
    @AcronymMap(value = "RRT", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Integer respiratoryRate;

    @JsonPathProperty(path = "$.['ST.Set.TargetRate', 'ST.Val.TargetRate']")
    @AcronymMap(value = "TBR",  useConstructor = true, clazz = BigDecimal.class)
    @Getter
    @Setter
    private BigDecimal targetRespiratoryRate;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.['ST.Set.IBREnable', 'ST.Val.IBREnable']")
    @AcronymMap(value = "VBE", clazz = SettingsIBREnable.class, method = "fromSettingsIBREnable")
    @Getter
    @Setter
    private IBREnable intelligentBackupRateEnabled;

    @JsonPathProperty(path = "$.['iVAPS.Set.EPAP', 'iVAPS.Val.EPAP']")
    @AcronymMap(value = "EPI", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Double epapIvaps;

    @JsonPathProperty(path = "$.['iVAPS.Set.TargetRate', 'iVAPS.Val.TargetRate' " +
                ", 'Val.TargetRate']")
    @AcronymMaps({
              @AcronymMap(value = "IBR", clazz = String.class, method = "valueOf"),
              @AcronymMap(value = "TPR", clazz = String.class, method = "valueOf")})
    @Getter
    @Setter
    private Integer targetPatientRate;

    @JsonPathProperty(path = "$.['iVAPS.Set.TargetAlveolarVent', 'iVAPS.Val.TargetAlveolarVent'," +
            "'Val.TargetAlveolarVent']")
    @AcronymMap(value = "WMV", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Double targetAlveolarVentilation;

    @JsonPathProperty(path = "$.['iVAPS.Set.MinPS', 'iVAPS.Val.MinPS', 'Val.iVAPS.MinPS']")
    @AcronymMaps({
            @AcronymMap(value = "WPM", clazz = String.class, method = "valueOf"),
            @AcronymMap(value = "LPI", clazz = String.class, method = "valueOf")})
    @Getter
    @Setter
    private Double minPsIvaps;

    @JsonPathProperty(path = "$.['iVAPS.Set.MaxPS', 'iVAPS.Val.MaxPS', 'Val.iVAPS.MaxPS']")
    @AcronymMaps({
            @AcronymMap(value = "WPA", clazz = String.class, method = "valueOf"),
            @AcronymMap(value = "ZZA", clazz = String.class, method = "valueOf")})
    @Getter
    @Setter
    private Double maxPsIvaps;

    @JsonPathProperty(path = "$.['iVAPS.Set.MinEPAP', 'iVAPS.Val.MinEPAP', 'Val.MinEPAP']")
    @AcronymMap(value = "IMN", clazz = String.class, method = "valueOf")
    @AcronymMaps({
            @AcronymMap(value = "IMN", clazz = String.class, method = "valueOf"),
            @AcronymMap(value = "MAP", clazz = String.class, method = "valueOf")})
    @Getter
    @Setter
    private Double minEpapIvaps;

    @JsonPathProperty(path = "$.['iVAPS.Set.MaxEPAP', 'iVAPS.Val.MaxEPAP', 'Val.MaxEPAP']")
    @AcronymMap(value = "IMX", clazz = String.class, method = "valueOf")
    @AcronymMaps({
            @AcronymMap(value = "IMX", clazz = String.class, method = "valueOf"),
            @AcronymMap(value = "XAP", clazz = String.class, method = "valueOf")})
    @Getter
    @Setter
    private Double maxEpapIvaps;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.['iVAPS.Set.AutoEPAP', 'iVAPS.Val.AutoEPAP', 'Val.AutoEPAP']")
    @AcronymMaps({
            @AcronymMap(value = "IEU", clazz = SettingsAutoEpap.class, method = "fromSettingsAutoEpap"),
            @AcronymMap(value = "AEE", clazz = SettingsAutoEpap.class, method = "fromSettingsAutoEpap")})
    @Getter
    @Setter
    private AutoEpap autoEpapEnabled;

    @JsonPathProperty(path = "$.['ASVAuto.Set.MinEPAP', 'ASVAuto.Val.MinEPAP']")
    @AcronymMap(value = "EAI", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Double minEpapAsvAuto;

    @JsonPathProperty(path = "$.['ASVAuto.Set.MaxEPAP', 'ASVAuto.Val.MaxEPAP']")
    @AcronymMap(value = "EAX", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Double maxEpapAsvAuto;

    @JsonPathProperty(path = "$.['ASVAuto.Set.MinPS', 'ASVAuto.Val.MinPS']")
    @AcronymMap(value = "ANS", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Double minPsAsvAuto;

    @JsonPathProperty(path = "$.['ASVAuto.Set.MaxPS', 'ASVAuto.Val.MaxPS']")
    @AcronymMap(value = "AXS", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Double maxPsAsvAuto;

    @Enumerated(STRING)
    @JsonPathProperty(path = "$.['Val.Mask']")
    @AcronymMap(value = "MSK", clazz = SettingsMask.class, method = "fromSettingsMask")
    @Getter
    @Setter
    private MaskCategory maskCategory;

    @OneToOne(fetch = FetchType.LAZY, cascade = CascadeType.ALL, orphanRemoval = true)
    @JoinColumn(name = "flow_gen_ventilator_settings_id")
    @Getter
    @Setter
    @JsonIgnore
    private FlowGenVentilatorSettings flowGenVentilatorSettings;

    @JsonPathProperty(path = "$.['HFT.Set.Flow', 'HFT.Val.Flow','Val.Flow']")
    @AcronymMap(value = "HFF", clazz = String.class, method = "valueOf")
    @Getter
    @Setter
    private Double hftFlow;

    @JsonPathProperty(path = "$.['Spont.Set.RespRateEnable', 'Spont.Val.RespRateEnable']")
    @AcronymMap(value = "BRE", clazz = SettingsRespRateEnable.class, method = "getValue", useClazz = true)
    @Getter
    @Setter
    private String respiratoryRateEnabled;

    @Transient
    private static Map<String, SettingsAcronymMapper> acronymMap = null;

    private static Map<String, SettingsAcronymMapper> getAcronymMap() {
        if (acronymMap == null) {
            acronymMap = SettingsAcronymUtil.getSettingsAcronymMapper(FlowGenSettings.class);
        }
        return acronymMap;
    }

    public static SettingsAcronymMapper getAcronymMapper(String acronym) {
        return getAcronymMap().get(acronym);
    }

    public FlowGenSettings(TherapyMode therapyMode) {
        this.therapyMode = therapyMode;
    }

    @JsonSerialize(using = JsonDoubleSerializer.class)
    public Double getSetPressure() {
        return setPressure;
    }

    public void setSetPressure(Double setPressure) {
        this.setPressure = setPressure;
    }

    @JsonSerialize(using = JsonDoubleSerializer.class)
    public Double getMinimumPressure() {
        return minimumPressure;
    }

    public void setMinimumPressure(Double minimumPressure) {
        this.minimumPressure = minimumPressure;
    }

    @JsonSerialize(using = JsonDoubleSerializer.class)
    public Double getMaximumPressure() {
        return maximumPressure;
    }

    public void setMaximumPressure(Double maximumPressure) {
        this.maximumPressure = maximumPressure;
    }

    public void setEprLevel(EprLevel eprLevel) {
        this.eprLevel = eprLevel;
    }

    public void setEprLevel(String value) {
        setEprLevel(EprLevel.fromCalOrEcoString(value));
    }

    @PostLoad
    public void onPostLoadUpdateRampDownEnable() {
        updateRampDownEnableAfterLoad();
    }

    /* this method is currently used just by Exchange */
    public void setRampDownEnableView(RampDownEnable newRampDownEnableView) {
        updateRampDownEnable(newRampDownEnableView);
    }

    public void updateRampDownEnable(RampDownEnable newRampDownEnable) {
        this.rampDownEnableView = newRampDownEnable;
        this.rampDownEnable = RampDownEnable.convertToPersistence(newRampDownEnable);
    }

    private void updateRampDownEnableAfterLoad() {
        this.rampDownEnableView = RampDownEnable.convertFromPersistence(this.rampDownEnable);
    }

    public void updateRampDownEnableBeforeSave() {
        this.rampDownEnable = RampDownEnable.convertToPersistence(this.rampDownEnableView);
    }

    public Object retrieveValue(TherapyDeviceSettingEnum enumToReturn) {
        switch (enumToReturn) {
            case MAX_PRESSURE:
                return getMaximumPressure();
            case MIN_PRESSURE:
                return getMinimumPressure();
            case EPR_LEVEL:
            case EPR_LEVEL_S10: return getEprLevel();
            case EPR_MODE: return getEprMode();
            case EPR_ENABLE: return null;
            case COMFORT_MODE:
                return getComfortMode();
            case DESAT_RULE_THRESHOLD:
                return getDesatRuleThreshold();
            case DEVICE_MODE:
            case DEVICE_ENHANCED_MODE:
                return getTherapyMode();
            case PRESSURE:
                return getSetPressure();
            case EASY_BREATHE:
                return getEasyBreathe();
            case RESP_RATE_ENABLE:
                return getRespiratoryRateEnabled();
            case IPAP:
                return getInspirationPressure();
            case EPAP:
                return getExpirationPressure();
            case MIN_EPAP:
                return getMinEpap();
            case MAX_IPAP:
                return getMaxIpap();
            case PRESSURE_SPT:
                return getPressureSupport();
            case BACKUP_RATE:
                return getBackupRate();
            case RESPIRATORY_RATE:
            case RESP_RATE:
                return getRespiratoryRate();
            case MIN_PS:
                return getMinPs();
            case MAX_PS:
                return getMaxPs();
            case EPAPASV:
                return getEpapasv();
            case EPAP_IVAPS:
                return getEpapIvaps();
            case MIN_PS_IVAPS:
            case IVAPS_MIN_PS:
                return getMinPsIvaps();
            case MAX_PS_IVAPS:
            case IVAPS_MAX_PS:
                return getMaxPsIvaps();
            case TARGET_PATIENT_RATE:
            case TARGET_RATE:
                return getTargetPatientRate();
            case TARGET_RESPIRATORY_RATE:
                return getTargetRespiratoryRate();
            case IBR_ENABLE:
                return getIntelligentBackupRateEnabled();
            case TARGET_ALVEOLAR_VENTILATION:
                return getTargetAlveolarVentilation();
            case MIN_EPAP_ASVAUTO:
                return getMinEpapAsvAuto();
            case MAX_EPAP_ASVAUTO:
                return getMaxEpapAsvAuto();
            case MIN_PS_ASVAUTO:
                return getMinPsAsvAuto();
            case MAX_PS_ASVAUTO:
                return getMaxPsAsvAuto();
            case AUTO_EPAP:
                return getAutoEpapEnabled();
            case MIN_EPAP_IVAPS:
                return getMinEpapIvaps();
            case MAX_EPAP_IVAPS:
                return getMaxEpapIvaps();
            case CIRCUIT:
                return getFlowGenVentilatorSettings().getCircuit();
            case INTERFACE:
                return getFlowGenVentilatorSettings().getPatientInterface();
            case MASK:
                return getMaskCategory();
            case PRESS_ASSIST:
                return getFlowGenVentilatorSettings().getPressureAssist();
            case SAFETY_VOLUME:
                return getFlowGenVentilatorSettings().getSafetyTidalVolume();
            case TIDAL_VOLUME:
                return getFlowGenVentilatorSettings().getTidalVolume();
            case INSPIRATORY_TIME:
                return getFlowGenVentilatorSettings().getInspiratoryTime();
            case STELLAR_INSPIRATORY_TIME:
                return getFlowGenVentilatorSettings().getInspiratoryTime();
            case MINIMUM_INSPIRATORY_TIME:
                return getFlowGenVentilatorSettings().getMinInspiratoryTime();
            case MAXIMUM_INSPIRATORY_TIME:
                return getFlowGenVentilatorSettings().getMaxInspiratoryTime();
            case RISE_TIME:
                return getFlowGenVentilatorSettings().getRiseTime();
            case TRIGGER_SENSITIVITY:
                return getFlowGenVentilatorSettings().getTriggerSensitivity();
            case CYCLE_SENSITIVITY:
                return getFlowGenVentilatorSettings().getCycleSensitivity();
            case TRIGGER_THRESHOLD:
                return getFlowGenVentilatorSettings().getTriggerThreshold();
            case TRIGGER_TYPE:
                return getFlowGenVentilatorSettings().getTriggerType();
            case CYCLE_THRESHOLD_PERCENT:
                return getFlowGenVentilatorSettings().getCycleThreshold();
            case PATIENT_TYPE:
                return getFlowGenVentilatorSettings().getPatientType();
            case PATIENT_HEIGHT:
                return getFlowGenVentilatorSettings().getHeight();
            case DURATION_OPTION:
                return getFlowGenVentilatorSettings().getVolumeBreathOptions();
            case FLOW_SHAPE:
                return getFlowGenVentilatorSettings().getFlowShape();
            case MANUAL_BREATH_ENABLE:
                return getFlowGenVentilatorSettings().getManualBreath();
            case MANUAL_MAGNITUDE_PERCENT:
                return getFlowGenVentilatorSettings().getManualMagnitude();
            case PEAK_FLOW:
                return getFlowGenVentilatorSettings().getPeakFlow();
            case SIGH_ALERT:
                return getFlowGenVentilatorSettings().getSighAlert();
            case SIGH_ENABLE:
                return getFlowGenVentilatorSettings().getSighEnable();
            case SIGH_INTERVAL:
                return getFlowGenVentilatorSettings().getSighInterval();
            case SIGH_MAGNITUDE_PERCENT:
                return getFlowGenVentilatorSettings().getSighMagnitude();
            case FALL_TIME:
                return getFlowGenVentilatorSettings().getFallTime();
            case RISE_ENABLE:
                return getFlowGenVentilatorSettings().getRiseEnable();
            case RAMP_ENABLE:
                return getRampEnable();
            case RAMP_DOWN_ENABLE:
                return getRampDownEnableView();
            case RAMP_TIME:
                return getRampTime();
            case START_EPAP:
            case START_EPAP_ASV:
            case START_EPAP_ASVAUTO:
            case START_EPAP_IVAPS:
            case START_EPAP_VAUTO:
                return getStartEpap();
            case START_PRESSURE:
            case START_PRESSURE_CPAP: return getStartPressure();
            case FLOW:
                return getHftFlow();
            default:
                throw new IllegalArgumentException("retrieveValue cannot handle " + enumToReturn);
        }
    }

    public void setValue(TherapyDeviceSettingEnum enumToSet, String value) {
        if (value == null) {
            return;
        }
        switch (enumToSet) {
            case MAX_PRESSURE:
                setMaximumPressure(Double.valueOf(value));
                break;
            case MIN_PRESSURE:
                setMinimumPressure(Double.valueOf(value));
                break;
            case EPR_LEVEL:
                setEprLevel(EprLevel.valueOf(value));
                break;
            case EPR_LEVEL_S10:
                setEprLevel(EprLevel.fromCalOrEcoString(value));
                break;
            case EPR_MODE:
                setEprMode(EprMode.valueOf(value));
                break;
            case COMFORT_MODE:
                setComfortMode(ComfortMode.valueOf(value));
                break;
            case DESAT_RULE_THRESHOLD:
                setDesatRuleThreshold(DesatRuleThreshold.valueOf(value));
                break;
            case DEVICE_MODE:
            case DEVICE_ENHANCED_MODE:
                setTherapyMode(TherapyMode.valueOf(value));
                break;
            case PRESSURE:
                setSetPressure(Double.valueOf(value));
                break;
            case EASY_BREATHE:
                setEasyBreathe(value);
                break;
            case RESP_RATE_ENABLE:
                setRespiratoryRateEnabled(value);
                break;
            case IPAP:
                setInspirationPressure(Double.valueOf(value));
                break;
            case EPAP:
                setExpirationPressure(Double.valueOf(value));
                break;
            case MIN_EPAP:
                setMinEpap(Double.valueOf(value));
                break;
            case MAX_IPAP:
                setMaxIpap(Double.valueOf(value));
                break;
            case PRESSURE_SPT:
                setPressureSupport(Double.valueOf(value));
                break;
            case BACKUP_RATE:
                setBackupRate(Double.valueOf(value));
                break;
            case RESPIRATORY_RATE:
            case RESP_RATE:
                setRespiratoryRate(Double.valueOf(value).intValue());
                break;
            case TARGET_RESPIRATORY_RATE:
                setTargetRespiratoryRate(new BigDecimal(value));
                break;
            case IBR_ENABLE:
                setIntelligentBackupRateEnabled(IBREnable.valueOf(value));
                break;
            case MIN_PS:
                setMinPs(Double.valueOf(value));
                break;
            case MAX_PS:
                setMaxPs(Double.valueOf(value));
                break;
            case EPAPASV:
                setEpapasv(Double.valueOf(value));
                break;
            case EPAP_IVAPS:
                setEpapIvaps(Double.valueOf(value));
                break;
            case MIN_PS_IVAPS:
            case IVAPS_MIN_PS:
                setMinPsIvaps(Double.valueOf(value));
                break;
            case MAX_PS_IVAPS:
            case IVAPS_MAX_PS:
                setMaxPsIvaps(Double.valueOf(value));
                break;
            case TARGET_PATIENT_RATE:
            case TARGET_RATE:
                setTargetPatientRate(Double.valueOf(value).intValue());
                break;
            case TARGET_ALVEOLAR_VENTILATION:
                setTargetAlveolarVentilation(Double.valueOf(value));
                break;
            case MIN_EPAP_ASVAUTO:
                setMinEpapAsvAuto(Double.valueOf(value));
                break;
            case MAX_EPAP_ASVAUTO:
                setMaxEpapAsvAuto(Double.valueOf(value));
                break;
            case MIN_PS_ASVAUTO:
                setMinPsAsvAuto(Double.valueOf(value));
                break;
            case MAX_PS_ASVAUTO:
                setMaxPsAsvAuto(Double.valueOf(value));
                break;
            case AUTO_EPAP:
                setAutoEpapEnabled(AutoEpap.valueOf(value));
                break;
            case MIN_EPAP_IVAPS:
                setMinEpapIvaps(Double.valueOf(value));
                break;
            case MAX_EPAP_IVAPS:
                setMaxEpapIvaps(Double.valueOf(value));
                break;
            case CIRCUIT:
                getFlowGenVentilatorSettings().setCircuit(Circuit.valueOf(value));
                break;
            case INTERFACE:
                getFlowGenVentilatorSettings().setPatientInterface(PatientInterface.valueOf(value));
                break;
            case MASK:
                setMaskCategory(MaskCategory.valueOf(value));
                break;
            case PRESS_ASSIST:
                getFlowGenVentilatorSettings().setPressureAssist(new BigDecimal(value));
                break;
            case SAFETY_VOLUME:
                getFlowGenVentilatorSettings().setSafetyTidalVolume(new BigDecimal(value));
                break;
            case TIDAL_VOLUME:
                getFlowGenVentilatorSettings().setTidalVolume(new BigDecimal(value));
                break;
            case INSPIRATORY_TIME:
                getFlowGenVentilatorSettings().setInspiratoryTime(new BigDecimal(value));
                break;
            case STELLAR_INSPIRATORY_TIME:
                getFlowGenVentilatorSettings().setInspiratoryTime(new BigDecimal(value));
                break;
            case MINIMUM_INSPIRATORY_TIME:
                getFlowGenVentilatorSettings().setMinInspiratoryTime(new BigDecimal(value));
                break;
            case MAXIMUM_INSPIRATORY_TIME:
                getFlowGenVentilatorSettings().setMaxInspiratoryTime(new BigDecimal(value));
                break;
            case RISE_TIME:
                getFlowGenVentilatorSettings().setRiseTime(new BigDecimal(value));
                break;
            case TRIGGER_SENSITIVITY:
                getFlowGenVentilatorSettings().setTriggerSensitivity(TriggerSensitivity.valueOf(value));
                break;
            case CYCLE_SENSITIVITY:
                getFlowGenVentilatorSettings().setCycleSensitivity(CycleSensitivity.valueOf(value));
                break;
            case TRIGGER_THRESHOLD:
                getFlowGenVentilatorSettings().setTriggerThreshold(new BigDecimal(value));
                break;
            case TRIGGER_TYPE:
                getFlowGenVentilatorSettings().setTriggerType(TriggerType.valueOf(value));
                break;
            case CYCLE_THRESHOLD_PERCENT:
                getFlowGenVentilatorSettings().setCycleThreshold(new BigDecimal(value));
                break;
            case PATIENT_TYPE:
                getFlowGenVentilatorSettings().setPatientType(PatientType.valueOf(value));
                break;
            case PATIENT_HEIGHT:
                getFlowGenVentilatorSettings().setHeight(new BigDecimal(value));
                break;
            case DURATION_OPTION:
                getFlowGenVentilatorSettings().setVolumeBreathOptions(VolumeBreathOptions.valueOf(value));
                break;
            case FLOW_SHAPE:
                getFlowGenVentilatorSettings().setFlowShape(FlowShape.valueOf(value));
                break;
            case MANUAL_BREATH_ENABLE:
                getFlowGenVentilatorSettings().setManualBreath(ManualBreath.valueOf(value));
                break;
            case MANUAL_MAGNITUDE_PERCENT:
                getFlowGenVentilatorSettings().setManualMagnitude(new BigDecimal(value));
                break;
            case PEAK_FLOW:
                getFlowGenVentilatorSettings().setPeakFlow(new BigDecimal(value));
                break;
            case SIGH_ALERT:
                getFlowGenVentilatorSettings().setSighAlert(SighAlert.valueOf(value));
                break;
            case SIGH_ENABLE:
                getFlowGenVentilatorSettings().setSighEnable(SighEnable.valueOf(value));
                break;
            case SIGH_INTERVAL:
                getFlowGenVentilatorSettings().setSighInterval(new BigDecimal(value));
                break;
            case SIGH_MAGNITUDE_PERCENT:
                getFlowGenVentilatorSettings().setSighMagnitude(new BigDecimal(value));
                break;
            case FALL_TIME:
                getFlowGenVentilatorSettings().setFallTime(new BigDecimal(value));
                break;
            case RISE_ENABLE:
                getFlowGenVentilatorSettings().setRiseEnable(RiseEnable.valueOf(value));
                break;
            case RAMP_ENABLE:
                setRampEnable(RampEnable.valueOf(value));
                break;
            case RAMP_DOWN_ENABLE:
                updateRampDownEnable(RampDownEnable.fromCalOrEcoString(value));
                break;
            case RAMP_TIME:
                setRampTime(Double.valueOf(value).intValue());
                break;
            case START_EPAP:
            case START_EPAP_ASV:
            case START_EPAP_ASVAUTO:
            case START_EPAP_IVAPS:
            case START_EPAP_VAUTO:
                setStartEpap(new BigDecimal(value));
                break;
            case START_PRESSURE:
            case START_PRESSURE_CPAP:
                setStartPressure(new BigDecimal(value));
                break;
            case FLOW:
                setHftFlow(Double.valueOf(value));
            default:
                throw new IllegalArgumentException("setValue cannot handle " + enumToSet);
        }
    }

    public boolean isPlaceholder() {
        return (PLACEHOLDER_VAL.equals(this.isPlaceholder));
    }

    public void setPlaceholder(boolean placeholder) {
        isPlaceholder = placeholder ? PLACEHOLDER_VAL : null;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof FlowGenSettings) {
            FlowGenSettings other = (FlowGenSettings) obj;
            return Objects.equal(backupRate, other.backupRate)
                    && Objects.equal(comfortMode, other.comfortMode)
                    && Objects.equal(easyBreathe, other.easyBreathe)
                    && Objects.equal(respiratoryRateEnabled, other.respiratoryRateEnabled)
                    && Objects.equal(epapIvaps, other.epapIvaps)
                    && Objects.equal(epapasv, other.epapasv)
                    && Objects.equal(eprLevel, other.eprLevel)
                    && Objects.equal(eprMode, other.eprMode)
                    && Objects.equal(expirationPressure, other.expirationPressure)
                    && Objects.equal(inspirationPressure, other.inspirationPressure)
                    && Objects.equal(isPlaceholder, other.isPlaceholder)
                    && Objects.equal(maxIpap, other.maxIpap)
                    && Objects.equal(maxPs, other.maxPs)
                    && Objects.equal(maxPsAsvAuto, other.maxPsAsvAuto)
                    && Objects.equal(maxPsIvaps, other.maxPsIvaps)
                    && Objects.equal(maximumPressure, other.maximumPressure)
                    && Objects.equal(minEpap, other.minEpap)
                    && Objects.equal(minEpapAsvAuto, other.minEpapAsvAuto)
                    && Objects.equal(minPs, other.minPs)
                    && Objects.equal(minPsAsvAuto, other.minPsAsvAuto)
                    && Objects.equal(minPsIvaps, other.minPsIvaps)
                    && Objects.equal(minimumPressure, other.minimumPressure)
                    && Objects.equal(pressureSupport, other.pressureSupport)
                    && Objects.equal(rampEnable, other.rampEnable)
                    && Objects.equal(rampTime, other.rampTime)
                    && Objects.equal(respiratoryRate, other.respiratoryRate)
                    && Objects.equal(targetRespiratoryRate, other.targetRespiratoryRate)
                    && Objects.equal(intelligentBackupRateEnabled, other.intelligentBackupRateEnabled)
                    && Objects.equal(setPressure, other.setPressure)
                    && Objects.equal(startPressure, other.startPressure)
                    && Objects.equal(targetAlveolarVentilation, other.targetAlveolarVentilation)
                    && Objects.equal(targetPatientRate, other.targetPatientRate)
                    && Objects.equal(therapyMode, other.therapyMode)
                    && Objects.equal(desatRuleThreshold, other.desatRuleThreshold)
                    && Objects.equal(autoEpapEnabled, other.autoEpapEnabled)
                    && Objects.equal(minEpapIvaps, other.minEpapIvaps)
                    && Objects.equal(maxEpapIvaps, other.maxEpapIvaps)
                    && Objects.equal(maskCategory, other.maskCategory)
                    && Objects.equal(rampDownEnable, other.rampDownEnable)
                    && Objects.equal(startEpap, other.startEpap)
                    && Objects.equal(hftFlow, other.hftFlow);

        }
        return false;
    }

    @Override
    public int hashCode() {
        return Objects.hashCode(backupRate, comfortMode, easyBreathe, respiratoryRateEnabled, epapIvaps, epapasv, eprLevel, eprMode,
                expirationPressure, inspirationPressure, isPlaceholder, maxIpap, maxPs, maxPsAsvAuto,
                maxPsIvaps, maximumPressure, minEpap, minEpapAsvAuto, minEpapAsvAuto, minPs, minPsAsvAuto, minPsIvaps, minimumPressure, pressureSupport,
                rampEnable, rampTime, respiratoryRate, targetRespiratoryRate, intelligentBackupRateEnabled, setPressure, startPressure,
                targetAlveolarVentilation, targetPatientRate, therapyMode, desatRuleThreshold, autoEpapEnabled, minEpapIvaps,
                maxEpapIvaps, maskCategory, rampDownEnable, startEpap, hftFlow);
    }
}
