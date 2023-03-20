import React from 'react'
import { ColorBar } from './ColorBar'
import { onmsColorArray } from './OnmsColors'

interface ColorThemeDisplayProps {
    theme: number | undefined | string;
}

export const ColorThemeDisplay: React.FC<ColorThemeDisplayProps> = ({ theme }) => {

    const getThemeNumberFromPossibleString = () => {
        let themeNum = 0;
        if (theme && typeof theme === 'string') {
            themeNum = Number(theme)
        } else if (theme && typeof theme === 'number' && !!onmsColorArray[theme]) {
            themeNum = theme;
        }
        return themeNum;
    }

    const titles = [
        { text: 'Indeterminate', icon: 'question-circle' },
        { text: 'Cleared', icon: 'check-circle' },
        { text: 'Normal', icon: 'leaf' },
        { text: 'Warning', icon: 'exclamation-triangle' },
        { text: 'Minor', icon: 'bolt' },
        { text: 'Major', icon: 'fire' },
        { text: 'Critical', icon: 'bomb' },
    ]

    return (
        <div
            style={{
                marginTop: 20,
            }}
        >
            {titles.map((title, index) => {
                return <ColorBar
                    key={index}
                    title={title.text}
                    icon={title.icon}
                    color={onmsColorArray[getThemeNumberFromPossibleString()][index]}
                />
            })}
        </div>
    )
}
