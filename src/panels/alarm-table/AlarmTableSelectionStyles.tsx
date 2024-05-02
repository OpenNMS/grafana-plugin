import React from 'react'

export const AlarmTableSelectionStyles = () => {
    return (
        <style>
            {
                `
                        .font-size-0 {
                            font-size:80%;
                        }
                        .font-size-1 {
                            font-size:90%;
                        }
                        .font-size-2 {
                            font-size:100%;
                        }
                        .font-size-3 {
                            font-size:110%;
                        }
                        .font-size-4 {
                            font-size:120%;
                        }
                        .font-size-5 {
                            font-size:130%;
                        }
                        .font-size-6 {
                            font-size:140%;
                        }
                        .font-size-7 {
                            font-size:150%;
                        }
                        .font-size-8 {
                            font-size:160%;
                        }
                        .font-size-9 {
                            font-size:180%;
                        }
                        .font-size-10 {
                            font-size:200%;
                        }
                        .font-size-11 {
                            font-size:220%;
                        }
                        .font-size-12 {
                            font-size:250%;
                        }
                        .no-scroll .track-horizontal{
                            display:none;
                        }
                        .alarm-query {
                            height:100%;
                        }
                        .non-alarm-query {
                            height:100%;
                            display:flex;
                            align-items:center;
                            justify-content:center;
                        }
                        .alarm-table-wrapper {
                            height: 90%;
                        }
                        .alarm-table-top-wrapper {
                            height: 90%;
                        }
                        .alarm-table-pagination-wrapper {
                            width: 100%;
                            height: 10%;
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
