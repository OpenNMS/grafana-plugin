import React from 'react'

export const AlarmTableSelectionStyles = () => {
    return (
        <style>
            {
                `
                        div[role="row"] {
                            border:2px solid transparent;
                            padding-bottom:1px;
                        }
                        .table-body div[role="row"]:hover {
                            background-color:rgb(1, 33, 137);
                        }
                        .table-body div[role="row"]:last-child {
                            border-bottom:1px solid transparent;
                        }
                        .table-body div[role="row"]:last-child {
                            border-bottom:1px solid transparent;
                        }

                        .table-body div[role="cell"],.table-body div[role="row"] {
                            user-select:none;
                        }
                        .table-body div[role="row"].select-start {
                            border-top:2px dashed white;
                            border-right:2px dashed white;
                            border-left:2px dashed white;
                        }
                        .table-body div[role="row"].select-continue {
                            border-right:2px dashed white;
                            border-left:2px dashed white;
                        }
                        .table-body div[role="row"].select-end {
                            border-bottom:2px dashed white;
                            border-right:2px dashed white;
                            border-left:2px dashed white;
                        }
 
                    `
            }
        </style>
    )
}
