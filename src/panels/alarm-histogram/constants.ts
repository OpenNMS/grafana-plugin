export const defaultColors = ['rgba(245, 54, 54, 0.9)', 'rgba(237, 129, 40, 0.89)', 'rgba(50, 172, 45, 0.97)']

export const AlarmDirections = {
    Vertical: { label: 'Vertical', value: 1 },
    Horizontal: { label: 'Horizontal', value: 2 }
}

export const AlarmGroups = {
    Acknowledged: { label: 'Acknowledged', value: 1 },
    Severity: { label: 'Severity', value: 2 },
}

export const AcknowledgedAlarms = {
    Outstanding: { index: 0, label: 'Outstanding', value: undefined },
    Acknowledged: { index: 1, label: 'Acknowledged', value: true },
}

export const AlarmSeverity = {
    CLEARED: { index: 0, label: 'Cleared', value: 'CLEARED' },
    NORMAL: { index: 1, label: 'Normal', value: 'NORMAL' },
    INDETERMINATE: { index: 2, label: 'Indeterm.', value: 'INDETERMINATE' },
    WARNING: { index: 3, label: 'Warning', value: 'WARNING' },
    MINOR: { index: 4, label: 'Minor', value: 'MINOR' },
    MAJOR: { index: 5, label: 'Major', value: 'MAJOR' },
    CRITICAL: { index: 6, label: 'Critical', value: 'CRITICAL' },
}
