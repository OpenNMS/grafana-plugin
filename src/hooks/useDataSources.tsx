import { getBackendSrv } from '@grafana/runtime';
import { useEffect, useState } from 'react';

interface GrafanaDatasource {name: string, type: string, id: string}
export const useDatasources = () => {
    const [datasources, setDatasources] = useState<GrafanaDatasource[]>([]);
    const updateDatasources = async () => {
        const data = await getBackendSrv().get('/api/datasources');
        setDatasources(data);
    }
    useEffect(() => {
        updateDatasources();
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])
    return { datasources }
}
