
import { OnmsEntityNestType, OnmsEntityType, SearchOption } from "./types"

export const defaultClause = {
    attribute: {} as unknown as SearchOption,
    comparator: {} as unknown as { l: string, i: number, aliases: string[] },
    comparedValue: '',
    comparedString: '',
    type: OnmsEntityType.FIRST,
    nestingType: OnmsEntityNestType.TOP
}

export const defaultOrder = { label: 'DESC' }
