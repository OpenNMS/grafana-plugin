import React from 'react'
import { css } from '@emotion/css'
import { InlineFieldRow, InlineLabel, useStyles2 } from '@grafana/ui'
import { GrafanaTheme2 } from '@grafana/data'

export interface SegmentSectionWithIconProps {
    label: string
    htmlFor?: string
    children: React.ReactNode
    fill?: boolean
    icon?: string
}

export const SegmentSectionWithIcon = ({ label, htmlFor, children, fill, icon }: SegmentSectionWithIconProps) => {
    const styles = useStyles2(getStyles);
    return (
        <>
            <InlineFieldRow>
                <InlineLabel htmlFor={htmlFor} width={10} className={`${styles.label} segment-with-icon`}>
                    {label}
                    {icon ? <i className={`fa fa-${icon}`} /> : null}
                </InlineLabel>
                {children}
                {fill && (
                    <div className={styles.fill}>
                        <InlineLabel>{''}</InlineLabel>
                    </div>
                )}
            </InlineFieldRow>
        </>
    )
}

const getStyles = (theme: GrafanaTheme2) => ({
    label: css`
      color: ${theme.colors.primary.text};
    `,
    fill: css`
      flex-grow: 1;
      margin-bottom: ${theme.spacing(0.5)};
    `,
})
