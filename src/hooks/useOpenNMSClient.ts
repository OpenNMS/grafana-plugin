import { DataSourceRef } from "@grafana/data"
import { getDataSourceSrv } from "@grafana/runtime"
import { ClientDelegate } from "lib/client_delegate"
import { useEffect, useState } from 'react'

export const useOpenNMSClient = (datasource: DataSourceRef | null | undefined) => {

    const [client, setClient] = useState<ClientDelegate>()

    useEffect(() => {

        const updateDatasource = async () => {
            const datasources = getDataSourceSrv()
            const datasourceObject: any = await datasources.get(datasource)
            setClient(datasourceObject.client);
        }

        if (datasource) {
            updateDatasource();
        }

    }, [datasource])

    return { client }
}
