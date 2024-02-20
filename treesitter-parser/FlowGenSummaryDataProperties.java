package easycare.model;

import easycare.model.json.JsonPathProperty;
import lombok.Getter;
import lombok.Setter;

import javax.persistence.Column;
import javax.persistence.Embeddable;
import java.math.BigDecimal;
import java.math.BigInteger;

@Getter
@Setter
@Embeddable
public class FlowGenSummaryDataProperties {
    private static final int THERAPY_DATA_PRECISION = 12;
    private static final int THERAPY_DATA_SCALE = 3;
    private static final int THERAPY_PERCENT_PRECISION = 5;
    private static final int THERAPY_PERCENT_SCALE = 2;

    @Column
    @JsonPathProperty(path = "$.['Val.ProgUsage']")
    private Integer programUsageMinutes;

    @JsonPathProperty(path = "$.['Val.AHI']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal apneaHypopneaIndex;

    @JsonPathProperty(path = "$.['Val.AI']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal apneaIndex;

    @JsonPathProperty(path = "$.['Val.HI']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal hypopneaIndex;

    /** Also called Central Apnea Index */
    @JsonPathProperty(path = "$.['Val.CAI']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal openApneaIndex;

    /** Also called Obstructive Apnea Index */
    @JsonPathProperty(path = "$.['Val.OAI']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal closedApneaIndex;

    @JsonPathProperty(path = "$.['Val.UAI']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal unknownApneaIndex;

    @JsonPathProperty(path = "$.['Val.ODI']")
    @Column(name = "oxygen_desat_index", precision = 5, scale = 2)
    private Double oxygenDesaturationIndex;

    @JsonPathProperty(path = "$.['Val.Leak.5', 'Val.LeakVented.5']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal maskLeak5thPercentile;

    @JsonPathProperty(path = "$.['Val.Leak.50', 'Val.LeakVented.50']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal maskLeakMedian;

    @JsonPathProperty(path = "$.['Val.Leak.95', 'Val.LeakVented.95']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal maskLeak95thPercentile;

    @JsonPathProperty(path = "$.['Val.Leak.Max']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal maskLeakMaximum;

    @JsonPathProperty(path = "$.['Val.MinVent.5']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal minuteVentilation5thPercentile;

    @JsonPathProperty(path = "$.['Val.MinVent.50']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal minuteVentilationMedian;

    @JsonPathProperty(path = "$.['Val.MinVent.95']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal minuteVentilation95thPercentile;

    @JsonPathProperty(path = "$.['Val.MinVent.Max']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal minuteVentilationMaximum;

    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal targetMinuteVentilationMedian;
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal targetMinuteVentilation95thPercentile;
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal targetMinuteVentilationMaximum;

    @JsonPathProperty(path = "$.['Val.RespRate.5']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal respiratoryRate5thPercentile;

    @JsonPathProperty(path = "$.['Val.RespRate.50']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal respiratoryRateMedian;

    @JsonPathProperty(path = "$.['Val.RespRate.95']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal respiratoryRate95thPercentile;

    @JsonPathProperty(path = "$.['Val.RespRate.Max']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal respiratoryRateMaximum;

    @JsonPathProperty(path = "$.['Val.SpontCyc%']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal spontaneousCycleRatioPercent;

    @JsonPathProperty(path = "$.['Val.SpontTrig%']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal spontaneousTriggeredRatioPercent;

    @JsonPathProperty(path = "$.['Val.TidVol.5']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal tidalVolume5thPercentile;

    @JsonPathProperty(path = "$.['Val.TidVol.50']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal tidalVolumeMedian;

    @JsonPathProperty(path = "$.['Val.TidVol.95']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal tidalVolume95thPercentile;

    @JsonPathProperty(path = "$.['Val.TidVol.Max']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal tidalVolumeMaximum;

    @JsonPathProperty(path = "$.['Val.IERatio.5']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal ieRatio5thCentilePercent;

    @JsonPathProperty(path = "$.['Val.IERatio.50']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal ieRatioMedianPercent;

    @JsonPathProperty(path = "$.['Val.IERatio.95']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal ieRatio95thCentilePercent;

    @JsonPathProperty(path = "$.['Val.IERatio.Max']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal ieRatioMaximumPercent;

    @JsonPathProperty(path = "$.['Val.AlvMinVent.5']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal alvMinuteVent5thPercent;

    @JsonPathProperty(path = "$.['Val.AlvMinVent.50']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal alvMinuteVentMedian;

    @JsonPathProperty(path = "$.['Val.AlvMinVent.95']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal alvMinuteVent95thPercent;

    @JsonPathProperty(path = "$.['Val.AlvMinVent.Max']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal alvMinuteVentMax;

    @JsonPathProperty(path = "$.['Val.TgtIPAP.5']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal inspiratoryPressure5thPercentile;

    @JsonPathProperty(path = "$.['Val.TgtIPAP.50']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal inspiratoryPressureMedian;

    @JsonPathProperty(path = "$.['Val.TgtIPAP.95']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal inspiratoryPressure95thPercentile;

    @JsonPathProperty(path = "$.['Val.TgtIPAP.Max']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal inspiratoryPressureMaximum;

    @JsonPathProperty(path = "$.['Val.TgtEPAP.5']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal expiratoryPressure5thPercentile;

    @JsonPathProperty(path = "$.['Val.TgtEPAP.50']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal expiratoryPressureMedian;

    @JsonPathProperty(path = "$.['Val.TgtEPAP.95']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal expiratoryPressure95thPercentile;

    @JsonPathProperty(path = "$.['Val.TgtEPAP.Max']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal expiratoryPressureMaximum;

    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal maskPressure95thPercentile;
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal maskPressureMaximum;
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal maskPressureMedian;
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigInteger maskCount;

    @JsonPathProperty(path = "$.['Val.CSR']")
    @Column
    private Integer cheyneStokesRespirationMinutes;

    @JsonPathProperty(path = "$.['Val.RIN']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigDecimal respiratoryEventRelatedArousalIndex;

    @JsonPathProperty(path = "$.['Val.SpO2.50']")
    @Column(name = "oxygen_sat_median_pct", precision = THERAPY_PERCENT_PRECISION, scale = THERAPY_PERCENT_SCALE)
    private Double oxygenSaturationMedian;

    @JsonPathProperty(path = "$.['Val.SpO2.Min']")
    @Column(name = "oxygen_sat_min_pct", precision = THERAPY_PERCENT_PRECISION, scale = THERAPY_PERCENT_SCALE)
    private Double oxygenSaturationMin;

    @JsonPathProperty(path = "$.['Val.SpO2.95']")
    @Column(name = "oxygen_sat_95th_pct", precision = THERAPY_PERCENT_PRECISION, scale = THERAPY_PERCENT_SCALE)
    private Double oxygenSaturation95thPercentile;

    @JsonPathProperty(path = "$.['Val.SpO2.5']")
    @Column(name = "oxygen_sat_5th_percentile", precision = THERAPY_PERCENT_PRECISION, scale = THERAPY_PERCENT_SCALE)
    private Double oxygenSaturation5thPercentile;

    // the number of minutes the patient had an oxygen saturation of less than 88%
    @JsonPathProperty(path = "$.['Val.SpO2Thresh']")
    @Column(name = "oxygen_sat_threshold", precision = 6, scale = 2)
    private Double oxygenSaturationMinutesBelowThreshold;

    @JsonPathProperty(path = "$.['Val.SpO2ThreshDynamic']")
    @Column(name = "oxygen_sat_below_dynamic_threshold", precision = 8, scale = 3)
    private Double oxygenSaturationSecondsBelowDynamicThreshold;

    @JsonPathProperty(path = "$.['Val.LeakValved.5']")
    @Column(name = "valved_leak5th_percentile", precision = THERAPY_PERCENT_PRECISION, scale = THERAPY_PERCENT_SCALE)
    private BigDecimal valvedLeak5thPercentile;

    @JsonPathProperty(path = "$.['Val.LeakValved.50']")
    @Column(name = "valved_leak_median", precision = THERAPY_PERCENT_PRECISION, scale = THERAPY_PERCENT_SCALE)
    private BigDecimal valvedLeakMedian;

    @JsonPathProperty(path = "$.['Val.LeakValved.95']")
    @Column(name = "valved_leak95th_percentile", precision = THERAPY_PERCENT_PRECISION, scale = THERAPY_PERCENT_SCALE)
    private BigDecimal valvedLeak95thPercentile;

    @JsonPathProperty(path = "$.['Val.PeakInspPress.5']")
    @Column(name = "peak_inspiratory_pressure5th_percentile", precision = THERAPY_PERCENT_PRECISION, scale = THERAPY_PERCENT_SCALE)
    private BigDecimal peakInspirationPressure5thPercentile;

    @JsonPathProperty(path = "$.['Val.PeakInspPress.50']")
    @Column(name = "peak_inspiratory_pressure_median", precision = THERAPY_PERCENT_PRECISION, scale = THERAPY_PERCENT_SCALE)
    private BigDecimal peakInspirationPressureMedian;

    @JsonPathProperty(path = "$.['Val.PeakInspPress.95']")
    @Column(name = "peak_inspiratory_pressure95th_percentile", precision = THERAPY_PERCENT_PRECISION, scale = THERAPY_PERCENT_SCALE)
    private BigDecimal peakInspirationPressure95thPercentile;

    @JsonPathProperty(path = "$.['Val.EndExpPressure.5']")
    @Column(name = "end_expiratory_pressure5th_percentile", precision = THERAPY_PERCENT_PRECISION, scale = THERAPY_PERCENT_SCALE)
    private BigDecimal endExpiratoryPressure5thPercentile;

    @JsonPathProperty(path = "$.['Val.EndExpPressure.50']")
    @Column(name = "end_expiratory_pressure_median", precision = THERAPY_PERCENT_PRECISION, scale = THERAPY_PERCENT_SCALE)
    private BigDecimal endExpiratoryPressureMedian;

    @JsonPathProperty(path = "$.['Val.EndExpPressure.95']")
    @Column(name = "end_expiratory_pressure95th_percentile", precision = THERAPY_PERCENT_PRECISION, scale = THERAPY_PERCENT_SCALE)
    private BigDecimal endExpiratoryPressure95thPercentile;

    public boolean hasProgramUsage() {
        return programUsageMinutes != null && programUsageMinutes > 0;
    }
}
