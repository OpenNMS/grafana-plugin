
export const useAlarmHelmProperties = (oldProperties, alarmTableData) => {
    const filteredProps = { ...oldProperties }
    if (filteredProps) {
        
        // Allow background color for severity column.
        filteredProps.fields = filteredProps.fields.map((field) => {
            if (field.name === 'Severity') {
                field.config.custom = { displayMode: 'color-background' }
            }
            return field;
        })

        // Filter our columns according to our configured approved fields.
        filteredProps.fields = filteredProps.fields.filter((fil) => {
            let shouldIncludeThisField = true;
            if (alarmTableData) {
                shouldIncludeThisField = !!alarmTableData.columns?.find((col) => col.label === fil.name)
            }
            return shouldIncludeThisField;
        })

        //Sort our columns based on the user provided order
        filteredProps.fields = filteredProps.fields.sort((f1,f2) => {
            const colIndex1 = alarmTableData.columns?.findIndex((col) => col.label === f1.name)
            const colIndex2 = alarmTableData.columns?.findIndex((col) => col.label === f2.name)
            return colIndex1 - colIndex2
        }) 
    }
    return filteredProps
}
