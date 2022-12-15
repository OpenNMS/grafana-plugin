import React from 'react'

export const AlarmTableSelectionStyles = () => {
    return (
        <style>
            {
                `
                        .alarm-query {
                            height:100%;
                        }
                        .non-alarm-query {
                            height:100%;
                            display:flex;
                            align-items:center;
                            justify-content:center;
                        }
                        div[role="row"] {
                            border:1px solid transparent;
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

                        .table-body div[role="row"].select-start {
                            border-top:1px dashed white;
                            border-right:1px dashed white;
                            border-left:1px dashed white;
                        }
                        .table-body div[role="row"].select-continue {
                            border-right:1px dashed white;
                            border-left:1px dashed white;
                        }
                        .table-body div[role="row"].select-end {
                            border-bottom:1px dashed white;
                            border-right:1px dashed white;
                            border-left:1px dashed white;
                        }
 
                    `
            }
        </style>
    )
}
