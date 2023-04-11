import { MetricFindValue } from '@grafana/data'
import { OnmsLocationResponse, OnmsFlowsResponse, OnmsResourceDto, OnmsResourceSelectQuery } from './api_types'
import { getMultiValues } from './utils'

export class SimpleOpenNMSRequest {
  backendSrv: any
  timeout = 10000
  url?: string
  withCredentials = false
  basicAuth?: string
  searchLimit = 25

  readonly flows = '/rest/flows'
  readonly locations = '/rest/monitoringLocations'
  readonly nodes = '/rest/nodes'
  readonly resources = '/rest/resources'

  constructor(backendSrv, url) {
    this.backendSrv = backendSrv
    this.url = url
  }

  doOpenNMSRequest(options: any): Promise<any> {
    if (this.basicAuth || this.withCredentials) {
      options.withCredentials = true
    }

    if (this.basicAuth) {
      options.headers = options.headers || {}
      options.headers.Authorization = this.basicAuth
    }

    options.url = this.url + options.url

    if (this.timeout) {
      options.timeout = this.timeout
    }

    return this.backendSrv.datasourceRequest(options)
  }

  getLocations = async (searchLimit = 0) => {
    const response = await this.doOpenNMSRequest({
      url: this.locations,
      method: 'GET',
      params: {
        limit: searchLimit
      }
    }) as OnmsLocationResponse

    if (response.data.count > response.data.totalCount) {
      console.warn(`Filter matches ${response.data.totalCount} records, but only ${response.data.count}  will be used.`)
    }

    const results: MetricFindValue[] = []

    response.data.location?.forEach(location => {
      const nodeLocation = location['location-name'] ? location['location-name'].toString() : null
      const exist = results.find(o => o.text === nodeLocation)

      if (nodeLocation && !exist) {
        results.push({ text: nodeLocation, value: nodeLocation, expandable: true })
      }
    })

    return results
  }

  async getNodesByFilter(filter: string) {
    const response = await this.doOpenNMSRequest({
      url: this.nodes,
      method: 'GET',
      params: {
        filterRule: filter,
        limit: 0
      }
    })

    if (response.data.count > response.data.totalCount) {
      console.warn(`Filter matches ${response.data.totalCount} records, but only ${response.data.count}  will be used.`)
    }

    return response.data.node
  }

  async getApplications(start: number, end: number, limit = 0) {
    const response = await this.doOpenNMSRequest({
      url: this.flows + '/applications/enumerate',
      method: 'GET',
      params: {
        'start': start,
        'end': end,
        'limit': limit <= 0 ? this.searchLimit : limit
      }
    }) as OnmsFlowsResponse

    const results: MetricFindValue[] = []

    if (response.data.length === 0) {
      console.warn('No matches found')
    } else {
      response.data.forEach(val => {
        results.push({ text: val, value: val, expandable: true })
      })
    }
    return results
  }

  async getHosts(start: number, end: number, pattern: string | null, limit = 0) {
    if (!pattern) {
      pattern = ".*"
    }

    const response = await this.doOpenNMSRequest({
      url: this.flows + '/hosts/enumerate',
      method: 'GET',
      params: {
        'start': start,
        'end': end,
        'limit': limit <= 0 ? this.searchLimit : limit,
        'pattern': pattern
      }
    }) as OnmsFlowsResponse

    if (response.data.length === 0) {
      return response.data
    } else {
      let results: any[] = []
      response.data.forEach(val => {
        results.push({ text: val, value: val, expandable: true })
      })

      return results
    }
  }

  async getConversations(start: number, end: number, application: string | null = null,
    location: string | null = null, protocol: string | null = null, limit = 0) {
    if (!application) {
      application = ".*"
    }
    if (!location) {
      location = ".*"
    }
    if (!protocol) {
      protocol = ".*"
    }

    const response = await this.doOpenNMSRequest({
      url: this.flows + '/conversations/enumerate',
      method: 'GET',
      params: {
        'start': start,
        'end': end,
        'application': application,
        'location': location,
        'protocol': protocol,
        'limit': limit <= 0 ? this.searchLimit : limit
      }
    }) as OnmsFlowsResponse

    const results: MetricFindValue[] = []

    if (response.data.length === 0) {
      console.warn("No matches found")
    } else {
      response.data.forEach(val => {
        results.push({ text: val, value: val, expandable: true })
      })
    }

    return results
  }

  async getNodeByIdOrFsFsId(query: string) {
    const response = await this.doOpenNMSRequest({
      url: this.nodes + '/' + query.trim(),
      method: 'GET',
      params: {
        limit: 0
      }
    })

    return response.data
  }

  /**
   * Get resources for a single node
   * @param nodeQuery needs to be used with getNodeAsResourceQuery()
   * @returns Array of resources if they exists for a node
   */
  async getResourcesForNode(nodeQuery: string) {
    const result: OnmsResourceDto[] = []

    const response = await this.doOpenNMSRequest({
      url: this.resources + '/' + encodeURIComponent(nodeQuery),
      method: 'GET',
      params: {
        depth: 1
      }
    })

    if (response.data.children.resource && Array.isArray(response.data.children.resource)) {
      result.push(...response.data.children.resource)
    }

    return result
  }

  /**
   * Get resources using the rest/resources/select endpoint
   * this allows:
   * - multiple nodes (comma separated id's)
   * - multiple resources (comma separated id's without node prefix
   * - multiple string properties if is necessary to narrow down
   * @param nodes 
   * @param nodeResources 
   * @param stringProperties 
   * @returns 
   */
  async getResourcesFor(nodes: string, nodeResources?: string, stringProperties?: string) {
    const nodesSet = new Set<string>()
    const nodeResourcesSet = new Set<string>()
    const stringPropertiesSet = new Set<string>()

    getMultiValues(nodes).forEach(n => nodesSet.add(n))
    if (nodeResources) {
      getMultiValues(nodeResources).forEach(n => nodeResourcesSet.add(n))
    }
    if (stringProperties) {
      getMultiValues(stringProperties).forEach(n => stringPropertiesSet.add(n))
    }

    return this.getResourcesForMultipleNodes(
      {
        nodes: nodesSet,
        nodeSubresources: nodeResourcesSet,
        stringProperties: stringPropertiesSet
      }
    )
  }

  /**
   * Get resources for multiple nodes as one array of OnmsResourceDto using the /rest/resources/select endpoint
   */
  async getResourcesForMultipleNodes(selection: OnmsResourceSelectQuery) {
    const response = await this.doOpenNMSRequest({
      url: '/rest/resources/select',
      method: 'GET',
      params: {
        nodes: Array.from(selection.nodes).join(','),
        nodeSubresources: Array.from(selection.nodeSubresources).join(','),
        stringProperties: Array.from(selection.stringProperties).join(',')
      }
    })

    const resources = response.data as OnmsResourceDto[]

    return resources.flatMap(r => r.children.resource)
  }
}
