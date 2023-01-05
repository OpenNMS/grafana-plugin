const getCircularReplacer = () => {
    const seen = new WeakSet();
    return (key, value) => {
        if (typeof value === "object" && value !== null) {
            if (seen.has(value)) {
                return;
            }
            seen.add(value);
        }
        return value;
    };
};

export const saveFilterPanel = (info: any) => {
    localStorage.setItem('opennms-helm-filter-panel', JSON.stringify(info, getCircularReplacer()))
}
