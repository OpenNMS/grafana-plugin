import React from 'react';

interface HelmColorThemeDisplayProps {
    theme: number | undefined | string;
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
const colors = [
    ['#999000', '#eee000', '#86b15b', '#fccc3b', '#ee901c', '#e3692f', '#db4345'],
    ['#999000', '#999', '#360', '#fc0', '#f90', '#f30', '#c00'],
    ['#614765', '#ccc', '#00b48b', '#00d3c9', '#ffce56', '#ff9033', '#ff3b50'],
    ['#999000', '#999', '#a1cc72', '#6ec5c6', '#f9df6c', '#dda35c'],
    ['#a44ae6', '#5dc93b', '#ccc', '#76b7f9', '#fffd54', '#f4b54b', '#eb3223']
]
const ColorBar: React.FC<{ title: string, icon: string, color: string }> = ({ title, icon, color }) => {
    return (<div style={{
        backgroundColor: color,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'space-between',
        width: '100%',
        height: 30,
        padding: '0 10px',
        color: '#fff',
        maxWidth: 260
    }} >
        <style>
            {
                `
                i.color-bar {
                    text-shadow: 1px 0 0 #000,-1px 0 0 #000,0 1px 0 #000,0 -1px 0 #000,.5px .5px #000,-.5px -.5px 0 #000,.5px -.5px 0 #000,-.5px .5px 0 #000;
                }
                .color-bar-title {
                    background-color:#565656;
                    padding:1px 8px;
                    border-radius: 10px;
                }
                `
            }
        </style>
        <div className={'color-bar-title'}>
            {title}
        </div>
        <div>
            <i className={` color-bar fa fa-${icon}`}></i>
        </div>
    </div>)
}
export const HelmColourThemeDisplay: React.FC<HelmColorThemeDisplayProps> = ({ theme }) => {
    const themeNumber = theme && typeof theme === 'string' ?
        Number(theme) :
        theme && typeof theme === 'number' ?
            theme :
            0;

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
                    color={colors[themeNumber][index]}
                />
            })}
        </div>
    )
}
