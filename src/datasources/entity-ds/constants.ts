
import { OnmsEntityClause, OnmsEntityNestType, OnmsEntityType, SearchOption } from "./types"

export const defaultClause = {
    attribute: {} as unknown as SearchOption,
    comparator: {} as unknown as { l: string, i: number, aliases: string[] },
    comparedValue: '',
    comparedString: '',
    type: OnmsEntityType.FIRST,
    nestingType: OnmsEntityNestType.TOP
} as OnmsEntityClause

export const defaultOrder = { label: 'DESC' }
