import { PanelProps } from '@grafana/data'
import React, { useEffect, useRef } from 'react'
import { NiceByteName } from './FlowHistogramContants';
import { getFlowHistogramPlotConfig, getFlowHistogramPlotData } from './FlowHistogramHelpers';
import { FlowHistogramOptionsProps } from './FlowHistogramTypes';

interface FlowHistogramControlOptions { flowHistogramOptions: FlowHistogramOptionsProps }
interface Props extends PanelProps<FlowHistogramControlOptions> { }

export const FlowHistogramControl: React.FC<Props> = ({ data, height, width, options }) => {
    const ref: any = useRef();
 
    useEffect(() => {
        const plotData = getFlowHistogramPlotData(data, options)
        const plotConfig = getFlowHistogramPlotConfig(options);

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
                `
                }
            </style>
            <div className={options.flowHistogramOptions.direction.label === 'Horizontal' ? 'side-spot' : ''} >
                <div ref={ref} style={{ width: width, height: height - 25 }} />
                <div className='side-spot-label'
                    style={{
                        height: 25,
                        display: 'flex',
                        justifyContent: 'center',
                        alignItems: 'center'
                    }}>
                    <p style={{ marginTop: 24 }}>{NiceByteName(options?.flowHistogramOptions?.units)}</p>
                </div>
            </div>
        </>
    )
}
