
export class ValueMapping {
    getApiValue(internalAttribute, value) {
        if (internalAttribute === 'alarmAckTime') {
            if (value === 'null') {
                value = new Date(0);
            }

            // yyyy-MM-ddTHH:mm:ss.sssZ
            // 2017-06-08T10:17:17.173+0200
            value = value.toJSON().replace("Z", "+0000"); // make it parsable by java
        }
        if ("null" === value) {
            value = '\u0000';
        }
        return value;
    }

    getUiValue(internalAttribute, value) {
        if (internalAttribute === 'alarmAckTime') {
            if (value === new Date(0)) {
                return 'null';
            }
            var stringified = JSON.parse(value.replace("+0000", "Z"));
            return new Date(stringified);
        }
        if (value === "\u0000") {
            return "null";
        }
        return value;
    }
}
