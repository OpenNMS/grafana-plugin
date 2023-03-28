import React from 'react'

export interface SwitchBoxProps {
    className?: string
    children: React.ReactNode
}

export const SwitchBox: React.FC<SwitchBoxProps> = ({className = undefined, children }) => {
    return (
        <div className={className} style={{ display: 'flex', alignItems: 'center', height: 32, width: 32 }}>
            {children}
        </div>
    )
}
