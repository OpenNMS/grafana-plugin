import { PanelProps } from '@grafana/data'
import React, { useEffect, useRef } from 'react'
import { UnitInfo } from './FlowHistogramContants';
import { getFlowHistogramPlotConfig, getFlowHistogramPlotData, getLabeledValues } from './FlowHistogramHelpers';
import { FlowHistogramOptionsProps } from './FlowHistogramTypes';
import _ from 'lodash';

interface FlowHistogramControlOptions { flowHistogramOptions: FlowHistogramOptionsProps }
interface Props extends PanelProps<FlowHistogramControlOptions> { }

export const FlowHistogramControl: React.FC<Props> = ({ data, height, width, options }) => {
    const ref: any = useRef();

    useEffect(() => {
        if (!data?.series || data.series.length === 0 || !data?.series[0].meta || !data?.series[0].meta.custom) {
            throw new Error('Incorrect or incomplete data, check the datasource is flow-datasource and function asSummaryTable are selected')
        }
        const processedData = getLabeledValues(data, options)
        const plotData = getFlowHistogramPlotData(processedData, options)
        const plotConfig = getFlowHistogramPlotConfig(processedData, options)

        $.plot(ref.current, plotData, plotConfig)

    }, [data, width, height, ref, options]);

    return (
        <>
            <style>
                {
                    `
                   .side-spot {
                        margin-left: 25px;
                        position: relative;
                    }                                     
                    .side-spot .side-spot-label {
                        transform: rotate(-90deg) translateY(-50%);
                        transform-origin: center;
                        top: 42%;
                        position: absolute;
                        width: 0;
                    }
                    .legendColorBox {
                        padding-right: 3px;
                    }
                    .legendLabel {
                        color: rgb(204, 204, 220);
                    }
                    .side-spot table {
                        height: ${options.flowHistogramOptions.height}px;
                    }
                    .flow-histogram-legend-right table {
                        margin: 0 auto;
                    }
                `
                }
            </style>
            <div>
                <div className={options.flowHistogramOptions.direction.label === 'Horizontal' ? '' : 'side-spot'}
                    style={{
                        display: (options.flowHistogramOptions.showLegend && options.flowHistogramOptions.position.label === 'Under Graph' ? 'block' : 'inline-block'),
                        float: (options.flowHistogramOptions.showLegend && options.flowHistogramOptions.position.label === 'Under Graph' ? 'none' : 'left')
                    }}>
                    <div ref={ref} style={{ 
                        width: (options.flowHistogramOptions.showLegend && options.flowHistogramOptions.position.label === 'Under Graph' ? width : width * 0.8),
                        height: height - 25 - (options.flowHistogramOptions.showLegend && options.flowHistogramOptions.position.label === 'Under Graph' ? options.flowHistogramOptions.height : 0 )}} />
                    <div className='side-spot-label'
                        style={{
                            height: 25,
                            display: 'flex',
                            justifyContent: 'center',
                            alignItems: 'center'
                        }}>
                        <p style={{ marginTop: 24 }}>{UnitInfo(options, data?.series).units}</p>
                    </div>
                </div>
                <div className={(options.flowHistogramOptions.position.label === 'Under Graph' ? 'flow-histogram-legend-bottom' : 'flow-histogram-legend-right')}
                    style={{
                        display: (options.flowHistogramOptions.showLegend ? (options.flowHistogramOptions.position.label === 'Under Graph' ? 'block' : 'inline-block') : 'none'),
                        marginTop: options.flowHistogramOptions.position.label === 'Under Graph' ? options.flowHistogramOptions.height : 0,
                        width: (options.flowHistogramOptions.showLegend && options.flowHistogramOptions.position.label === 'Under Graph' ? width : width * 0.2 - (options.flowHistogramOptions.direction.label === 'Horizontal' ? 0 : 25)),
                        height: (options.flowHistogramOptions.showLegend && options.flowHistogramOptions.position.label === 'Under Graph' ? options.flowHistogramOptions.height : height ), 
                        float: (options.flowHistogramOptions.showLegend && options.flowHistogramOptions.position.label === 'Under Graph' ? 'none' : 'left'),                        
                    }} />
            </div>
        </>
    )
}
