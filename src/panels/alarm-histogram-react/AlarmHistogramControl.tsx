import React, { useEffect } from 'react';

import $ from 'jquery'
import { useAlarmHooks } from './alarmhooks';
import { SimpleOptions } from 'types';
import { PanelProps } from '@grafana/data';

interface AlarmHistogramControlOptions extends SimpleOptions {alarmGroup: number, alarmDirection: number}
interface Props extends PanelProps<AlarmHistogramControlOptions> { }

export const AlarmHistogramControl: React.FC<Props> = ({ options, data, width, height }) => {

    const { getPlotConfig, getPlotData, ref } = useAlarmHooks();

    
    useEffect(() => {
        const plotData = getPlotData(options.alarmGroup, options.alarmDirection,data)
        const plotConfig = getPlotConfig(options.alarmGroup, options.alarmDirection);
        $.plot(ref.current, plotData, plotConfig)

    }, [data, options.alarmGroup, width, height, options.alarmDirection, getPlotConfig, getPlotData, ref]);

    return (
        <div ref={ref} style={{ width: width, height: height }} />
    )
}
