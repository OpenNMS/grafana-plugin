import { ClientDelegate } from "lib/client_delegate"
import { FlowFunction, SegmentOption } from "./types"

export enum FlowFunctionNames {
    asTableSummary,
    combineIngressEgress,
    includeOther,
    nanToZero,
    negativeEgress,
    negativeIngress,
    onlyEgress,
    onlyIngress,
    perSecond,
    swapIngressEgress,
    toBits,
    topN,
    withApplication,
    withConversation,
    withGroupByInterval,
    withHost,
    withDscp,
    withExporterNode,
    withIfIndex,
    withPrefix,
    withSuffix,
}

export const FlowFunctionStrings = {
    asTableSummary: 'asTableSummary',
    combineIngressEgress: 'combineIngressEgress',
    includeOther: 'includeOther',
    nanToZero: 'nanToZero',
    negativeEgress: 'negativeEgress',
    negativeIngress: 'negativeIngress',
    onlyEgress: 'onlyEgress',
    onlyIngress: 'onlyIngress',
    perSecond: 'perSecond',
    swapIngressEgress: 'swapIngressEgress',
    toBits: 'toBits',
    topN: 'topN',
    withApplication: 'withApplication',
    withConversation: 'withConversation',
    withGroupByInterval: 'withGroupByInterval',
    withHost: 'withHost',
    withDscp: 'withDscp',
    withExporterNode: 'withExporterNode',
    withIfIndex: 'withIfIndex',
    withPrefix: 'withPrefix',
    withSuffix: 'withSuffix',
}

export enum FlowSegments {
    Applications,
    Conversations,
    Hosts,
    Dscps,
}

export const FlowSegmentStrings = {
    Applications: 'Applications',
    Conversations: 'Conversations',
    Hosts: 'Hosts',
    Dscps: 'Dscps',
}

export const lowercaseEnumByValue = (fullEnum) => {

}

export const segmentOptionValues = [
    { value: FlowSegments.Applications, label: FlowSegments[FlowSegments.Applications].toLowerCase() },
    { value: FlowSegments.Conversations, label: FlowSegments[FlowSegments.Conversations].toLowerCase() },
    { value: FlowSegments.Hosts, label: FlowSegments[FlowSegments.Hosts].toLowerCase() },
    { value: FlowSegments.Dscps, label: FlowSegments[FlowSegments.Dscps].toLowerCase() },
]

export const includeOtherFunction: FlowFunction = {
    parentSegments: [
        FlowSegments.Applications,
        FlowSegments.Conversations,
        FlowSegments.Hosts,
    ]
}

export const withExporterNodeFunction: FlowFunction = {
    parentSegments: [
        FlowSegments.Applications,
        FlowSegments.Conversations,
        FlowSegments.Hosts,
    ]
}

export const topNFunction: FlowFunction = {
    excludeFunctions: [
        FlowFunctionNames.withApplication,
        FlowFunctionNames.withHost,
        FlowFunctionNames.withConversation
    ],
    parentSegments: [
        FlowSegments.Applications,
        FlowSegments.Conversations,
        FlowSegments.Hosts,
    ],
    parameter: "10"
}

export const withConversationFunction: FlowFunction = {
    excludeFunctions: [FlowFunctionNames.topN],
    parentSegments: [FlowSegments.Conversations],
    parameter: ''
}

export const withGroupByIntervalFunction: FlowFunction = {
    excludeFunctions: [FlowFunctionNames.asTableSummary],
    parameter: ''
}

export const withIfIndexFunction: FlowFunction = {
    parameter: "0"
}

export const withPrefixFunction: FlowFunction = {
    parameter: ''
}

export const withSuffixFunction: FlowFunction = {
    parameter: ''
}

export const withDscpFunction: FlowFunction = {
    allowMultiple: true,
    parameterOptions: async (input, client: ClientDelegate, start: number | undefined, end: number | undefined) => {
        return client.getDscpValues(false, undefined,start,end)
    }
}

export const withApplicationFunction: FlowFunction = {
    excludeFunctions: [FlowFunctionNames.topN],
    parentSegments: [FlowSegments.Applications],
    parameterOptions: async (input, client: ClientDelegate, start: number | undefined, end: number | undefined) => {
        return client.getApplications(input,start,end,false,undefined,undefined);
    }
}

export const withHostFunction: FlowFunction = {
    excludeFunctions: [FlowFunctionNames.topN],
    parentSegments: [FlowSegments.Hosts],
    parameterOptions: async (input, client: ClientDelegate, start: number | undefined, end: number | undefined) => {
      return client.getHosts(input, start,end,false,undefined,undefined);
    }
}

export const tableSummaryDefault = {
    excludeFunctions: [FlowFunctionNames.asTableSummary]
}

export const perSecondFunction: FlowFunction =
    { ...tableSummaryDefault }

export const nanToZeroFunction: FlowFunction =
    { ...tableSummaryDefault }

export const negativeEgressFunction: FlowFunction =
    { ...tableSummaryDefault }

export const combineIngressEgressFunction: FlowFunction =
    { ...tableSummaryDefault }

export const onlyIngressFunction: FlowFunction =
    { ...tableSummaryDefault }

export const onlyEgressFunction: FlowFunction =
    { ...tableSummaryDefault }


export const asTableSummaryFunction: FlowFunction = {
    excludeFunctions: [
        FlowFunctionNames.perSecond,
        FlowFunctionNames.negativeEgress,
        FlowFunctionNames.negativeIngress,
        FlowFunctionNames.combineIngressEgress,
        FlowFunctionNames.onlyIngress,
        FlowFunctionNames.onlyEgress,
        FlowFunctionNames.withGroupByInterval,
    ]
}


export const FlowFunctions = new Map([
    [FlowFunctionNames[FlowFunctionNames.topN], topNFunction],
    [FlowFunctionNames[FlowFunctionNames.includeOther], includeOtherFunction],
    [FlowFunctionNames[FlowFunctionNames.withExporterNode], withExporterNodeFunction],
    [FlowFunctionNames[FlowFunctionNames.withIfIndex], withIfIndexFunction],
    [FlowFunctionNames[FlowFunctionNames.withDscp], withDscpFunction],
    [FlowFunctionNames[FlowFunctionNames.withApplication], withApplicationFunction],
    [FlowFunctionNames[FlowFunctionNames.withHost], withHostFunction],
    [FlowFunctionNames[FlowFunctionNames.withConversation], withConversationFunction],
    [FlowFunctionNames[FlowFunctionNames.perSecond], perSecondFunction],
    [FlowFunctionNames[FlowFunctionNames.toBits], {}],
    [FlowFunctionNames[FlowFunctionNames.nanToZero], nanToZeroFunction],
    [FlowFunctionNames[FlowFunctionNames.swapIngressEgress], {}],
    [FlowFunctionNames[FlowFunctionNames.negativeEgress], negativeEgressFunction],
    [FlowFunctionNames[FlowFunctionNames.asTableSummary], asTableSummaryFunction],
    [FlowFunctionNames[FlowFunctionNames.combineIngressEgress], combineIngressEgressFunction],
    [FlowFunctionNames[FlowFunctionNames.onlyIngress], onlyIngressFunction],
    [FlowFunctionNames[FlowFunctionNames.onlyEgress], onlyEgressFunction],
    [FlowFunctionNames[FlowFunctionNames.withGroupByInterval], withGroupByIntervalFunction],
    [FlowFunctionNames[FlowFunctionNames.withPrefix], withPrefixFunction],
    [FlowFunctionNames[FlowFunctionNames.withSuffix], withSuffixFunction]
])

export const defaultSegmentOptions: SegmentOption[] = [
    {
        label: 'Combine',
        options: [
            { label: FlowFunctionNames[FlowFunctionNames.topN] },
            { label: FlowFunctionNames[FlowFunctionNames.includeOther] },
        ]
    },
    {
        label: 'Filter',
        options: [
            { label: FlowFunctionNames[FlowFunctionNames.withApplication] },
            { label: FlowFunctionNames[FlowFunctionNames.withDscp] },
            { label: FlowFunctionNames[FlowFunctionNames.withExporterNode] },
            { label: FlowFunctionNames[FlowFunctionNames.withIfIndex] },
            { label: FlowFunctionNames[FlowFunctionNames.withHost] },
            { label: FlowFunctionNames[FlowFunctionNames.withConversation] },
        ]
    },
    {
        label: 'Transform',
        options: [
            { label: FlowFunctionNames[FlowFunctionNames.asTableSummary] },
            { label: FlowFunctionNames[FlowFunctionNames.combineIngressEgress] },
            { label: FlowFunctionNames[FlowFunctionNames.nanToZero] },
            { label: FlowFunctionNames[FlowFunctionNames.negativeEgress] },
            { label: FlowFunctionNames[FlowFunctionNames.negativeIngress] },
            { label: FlowFunctionNames[FlowFunctionNames.onlyEgress] },
            { label: FlowFunctionNames[FlowFunctionNames.onlyIngress] },
            { label: FlowFunctionNames[FlowFunctionNames.perSecond] },
            { label: FlowFunctionNames[FlowFunctionNames.swapIngressEgress] },
            { label: FlowFunctionNames[FlowFunctionNames.toBits] },
            { label: FlowFunctionNames[FlowFunctionNames.withGroupByInterval] },
            { label: FlowFunctionNames[FlowFunctionNames.withPrefix] },
            { label: FlowFunctionNames[FlowFunctionNames.withSuffix] },
        ]
    }
]

export const segmentMapping = {
    [FlowSegmentStrings.Applications]: FlowFunctionStrings.withApplication,
    [FlowSegmentStrings.Conversations]: FlowFunctionStrings.withConversation,
    [FlowSegmentStrings.Hosts]: FlowFunctionStrings.withHost,
}

export const segmentFunctionMapping = {
    summaries: {
        [FlowSegmentStrings.Conversations]: {
            default: 'getSummaryForConversations',
            topN: 'getSummaryForTopNConversations'
        },
        [FlowSegmentStrings.Applications]: {
            default: 'getSummaryForApplications',
            topN: 'getSummaryForTopNApplications'
        },
        [FlowSegmentStrings.Hosts]: {
            default: 'getSummaryForHosts',
            topN: 'getSummaryForTopNHosts'
        },
        [FlowSegmentStrings.Dscps]: {
            default: 'getSummaryForDscps',
            topN: ''
        }
    },
    series: {
        [FlowSegmentStrings.Conversations]: {
            default: 'getSeriesForConversations',
            topN: 'getSeriesForTopNConversations',
        },
        [FlowSegmentStrings.Applications]: {
            default: 'getSeriesForApplications',
            topN: 'getSeriesForTopNApplications'
        },
        [FlowSegmentStrings.Hosts]: {
            default: 'getSeriesForHosts',
            topN: 'getSeriesForTopNHosts'
        },
        [FlowSegmentStrings.Dscps]: {
            default: 'getSeriesForDscps',
            topN: ''
        }
    }
}

export const Success = 'Success';
export const SuccessStatus = 'success';
export const Failure = 'Failure';
export const FailureStatus = 'failure';

export const summaries = 'summaries';
export const series = 'series';

export const FlowStrings = {
    Success,
    SuccessStatus,
    Failure,
    FailureStatus,
    summaries,
    series
}
