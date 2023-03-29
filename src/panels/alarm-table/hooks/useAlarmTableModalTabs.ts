import { useState } from 'react'

export const useAlarmTableModalTabs = () => {
    const [tabActive, setTabActive] = useState(0);

    const tabClick = (e) => {
        setTabActive(e);
    }

    const resetTabs = () => {
        setTabActive(0)
    }

    return { tabActive, tabClick, resetTabs }
}
