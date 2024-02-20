package easycare.model;

import static javax.persistence.EnumType.STRING;

import easycare.model.json.JsonPathProperty;

import java.math.BigInteger;
import javax.persistence.Column;
import javax.persistence.Embeddable;
import javax.persistence.Enumerated;

import lombok.Getter;
import lombok.Setter;

/** Climate specific data that comes with therapy summary data. */
@Getter
@Setter
@Embeddable
public class FlowGenClimateSummaryDataProperties {

    private static final int THERAPY_DATA_PRECISION = 12;
    private static final int THERAPY_DATA_SCALE = 3;

    @JsonPathProperty(path = "$.['Val.Humidifier']")
    @Enumerated(STRING)
    private HumidifierType humidifierType;

    @JsonPathProperty(path = "$.['Val.HeatedTube']")
    @Column
    private String heatedTubeType;

    @JsonPathProperty(path = "$.['Val.AmbHumidity']")
    @Column(precision = THERAPY_DATA_PRECISION, scale = THERAPY_DATA_SCALE)
    private BigInteger ambientAbsoluteHumidity;

}
