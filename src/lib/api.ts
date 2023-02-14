/**
 * Api namespace to add types for different onms rest api's
 */
export namespace OnmsApi {
    export namespace Measurements {
        export interface Resource {
            id: string,
            link?: string,
            name?: string,
            label?: string
        }
    }
}