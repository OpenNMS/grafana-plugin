import React, { useState } from 'react'
import { Button } from '@grafana/ui'
import { clearFilterEditorData } from '../lib/localStorageService'

export const ClearFilterData: React.FC<{}> = () => {
  const [filterDataCleared, setFilterDataCleared] = useState<boolean>(false)

  const clearFilterData = () => {
    clearFilterEditorData()
    setFilterDataCleared(true)
  }

  return (
    <>
      <style>
        {
          `
          .spacer {
            margin-top: 10px;
            margin-bottom: 10px;
          }
          `
      }
      </style>
      <h3 className='spacer'>Filter Data</h3>
      <div className='spacer'>
        OpenNMS Filter Panel data is stored in browser local storage.
        Click here to remove any existing filter data.
      </div>
      <Button
        onClick={() => clearFilterData()}
      >
        Clear Filter Data
      </Button>
      {
        filterDataCleared &&
        <div className='spacer'>Filter data was cleared.</div>
      }
    </>
  )
}
