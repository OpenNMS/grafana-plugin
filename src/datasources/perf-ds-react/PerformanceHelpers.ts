import { FunctionFormatter } from "lib/function_formatter";
import { SeriesResponse } from "./types";

export const timestampsToDatapoints = (timestamps: [], columns: [{ values: number | null[] }], i: number, { start, end }: { start: number, end: number }) => {

    let thereIsAtLeastOneValidValue;
    let datapoints: [[string, string]] = [['', '']]

    for (let j = 0; j < timestamps.length; j++) {
        if (timestamps[j] < start || timestamps[j] > end) {
            continue;
        }

        let columnValue = columns[i].values[j];
        if (columnValue === 'NaN') {
            columnValue = null;
        }

        if (!thereIsAtLeastOneValidValue && !isNaN(columnValue)) {
            thereIsAtLeastOneValidValue = true;
        }
        datapoints.push([columnValue, timestamps[j]]);
    }
    return { datapoints, thereIsAtLeastOneValidValue }
}

export const measurementResponseToGrafanaSeries = (response) => {
    const { labels, columns, timestamps, metadata } = response.data;

    let series: SeriesResponse = { target: '', label: '', datapoints: [['', '']] }
    for (let i = 0; i < columns.length; i++) {

        const { datapoints, thereIsAtLeastOneValidValue } = timestampsToDatapoints(timestamps, columns, i, response.data);

        let label = metadata && metadata.resources ?
            FunctionFormatter.format(labels[i], metadata) :
            labels[i];

        if (thereIsAtLeastOneValidValue) {
            series = {
                target: label,
                label: labels[i],
                datapoints: datapoints
            }
        }
    }
    return series;
}
