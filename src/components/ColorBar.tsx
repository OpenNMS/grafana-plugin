import React from 'react'

export const ColorBar: React.FC<{ title: string, icon: string, color: string }> = ({ title, icon, color }) => {
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
