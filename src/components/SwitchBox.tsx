import React from 'react'
export const SwitchBox: React.FC = ({children}) => {
    return (
        <div style={{display:'flex',alignItems:'center',height:32,width:32}}>
            {children}
        </div>
    )
}
