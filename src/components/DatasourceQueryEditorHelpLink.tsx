import React from 'react'
import { DATASOURCE_HELP_BASE_URL } from '../constants/constants'

interface DatasourceQueryEditorHelpLinkProps {
  datasourceName: string,
  relativeLink: string
}

export const DatasourceQueryEditorHelpLink: React.FC<DatasourceQueryEditorHelpLinkProps> =
  ({ datasourceName, relativeLink }) => {

  const docLink = `${DATASOURCE_HELP_BASE_URL}/${relativeLink}`

  return (
    <>
      <style>
          {
            `
              a.onms-doc-link {
                /* matches '.markdown-html' color */
                color: #6e9fff;
              }
              a.onms-doc-link:hover {
                text-decoration: underline;
              }
            `
          }
      </style>
      <div>
        See the <a className='onms-doc-link' href={docLink}>{datasourceName} Documentation</a> for help.
      </div>
    </>
  )
}
