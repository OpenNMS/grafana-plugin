export const DirectionOptions = [
    { label: 'Horizontal', value: '0' },
    { label: 'Vertical', value: '1' }
]
export const UnitOptions = [
    { label: 'B', value: '0' },
    { label: 'KB', value: '1' },
    { label: 'MB', value: '2' },
    { label: 'GB', value: '3' },
]

export const DisplayOptions = [
    { label: 'Total', value: '0' },
    { label: 'Rate', value: '1' },
]

export const ModeOptions = [
    { label: 'Separate', value: '0' },
    { label: 'Stacked', value: '1' },
]

export const PositionOptions = [
    { label: 'Top Left', value: 'nw' },
    { label: 'Bottom Left', value: 'sw' },
]

export const NiceByteName = (option) => {
    let name = 'Bytes'
    if (option.label === 'KB') {
        name = 'KiloBytes'
    } else if (option.label === 'MB') {
        name = 'MegaBytes'
    } else if (option.label === 'GB') {
        name = 'GigaBytes'
    }
    return name;
}
