import {
  normalizeBaseUrl,
  resolveOpenNMSLink,
  toOpenNMSRelativeUrl
} from '../../lib/externalLinks'
import { ClientDelegate } from '../../lib/client_delegate'

// The Grafana frontend rewrites a proxied datasource's url to this shape, and opennms-js
// builds Alarm.detailsPage by appending to it. See OnmsServer.resolveURL.
const PROXY_URL = '/api/datasources/proxy/uid/af2mze8mmd9moa'
const DETAILS_PAGE = `${PROXY_URL}/alarm/detail.htm?id=9849`

describe('ExternalLinks :: normalizeBaseUrl', () => {
  it('should return an empty string when no base url is given', () => {
    expect(normalizeBaseUrl(undefined)).toEqual('')
    expect(normalizeBaseUrl('')).toEqual('')
    expect(normalizeBaseUrl('   ')).toEqual('')
  })

  it('should trim surrounding whitespace', () => {
    expect(normalizeBaseUrl('  http://localhost:8980/opennms  ')).toEqual('http://localhost:8980/opennms')
  })

  it('should strip trailing slashes so a relative path does not produce a double slash', () => {
    expect(normalizeBaseUrl('http://localhost:8980/opennms/')).toEqual('http://localhost:8980/opennms')
    expect(normalizeBaseUrl('http://localhost:8980/opennms///')).toEqual('http://localhost:8980/opennms')
  })
})

describe('ExternalLinks :: toOpenNMSRelativeUrl', () => {
  it('should strip the Grafana datasource proxy prefix', () => {
    expect(toOpenNMSRelativeUrl(DETAILS_PAGE, PROXY_URL)).toEqual('/alarm/detail.htm?id=9849')
  })

  it('should strip the datasource url even when it has a trailing slash', () => {
    expect(toOpenNMSRelativeUrl(DETAILS_PAGE, `${PROXY_URL}/`)).toEqual('/alarm/detail.htm?id=9849')
  })

  it('should strip an absolute datasource url, as used in direct/browser access mode', () => {
    const url = 'http://host.docker.internal:8980/opennms/alarm/detail.htm?id=1'
    expect(toOpenNMSRelativeUrl(url, 'http://host.docker.internal:8980/opennms')).toEqual('/alarm/detail.htm?id=1')
  })

  it('should keep the url, with a leading slash, when it does not start with the datasource url', () => {
    expect(toOpenNMSRelativeUrl('alarm/detail.htm?id=7', PROXY_URL)).toEqual('/alarm/detail.htm?id=7')
    expect(toOpenNMSRelativeUrl('/alarm/detail.htm?id=7', '/some/other/base')).toEqual('/alarm/detail.htm?id=7')
  })

  it('should keep the url when there is no datasource url to strip', () => {
    expect(toOpenNMSRelativeUrl('/alarm/detail.htm?id=7', undefined)).toEqual('/alarm/detail.htm?id=7')
  })
})

describe('ExternalLinks :: resolveOpenNMSLink', () => {
  it('should return undefined when there is no url to resolve', () => {
    const options = { datasourceUrl: PROXY_URL, baseUrl: 'http://localhost:8980/opennms', enabled: true }

    expect(resolveOpenNMSLink(undefined, options)).toBeUndefined()
    expect(resolveOpenNMSLink('', options)).toBeUndefined()
    expect(resolveOpenNMSLink('   ', options)).toBeUndefined()
  })

  it('should build an absolute url when enabled with a base url', () => {
    const link = resolveOpenNMSLink(DETAILS_PAGE, {
      datasourceUrl: PROXY_URL,
      baseUrl: 'http://localhost:8980/opennms',
      enabled: true
    })

    expect(link).toEqual({ href: 'http://localhost:8980/opennms/alarm/detail.htm?id=9849', isAbsolute: true })
  })

  it('should not produce a double slash when the base url has a trailing slash', () => {
    const link = resolveOpenNMSLink(DETAILS_PAGE, {
      datasourceUrl: PROXY_URL,
      baseUrl: 'http://localhost:8980/opennms/',
      enabled: true
    })

    expect(link?.href).toEqual('http://localhost:8980/opennms/alarm/detail.htm?id=9849')
  })

  it('should fall back to the relative url when disabled, even if a base url is saved', () => {
    const link = resolveOpenNMSLink(DETAILS_PAGE, {
      datasourceUrl: PROXY_URL,
      baseUrl: 'http://localhost:8980/opennms',
      enabled: false
    })

    expect(link).toEqual({ href: '/alarm/detail.htm?id=9849', isAbsolute: false })
  })

  it('should fall back to the relative url when enabled but the base url is blank', () => {
    const link = resolveOpenNMSLink(DETAILS_PAGE, {
      datasourceUrl: PROXY_URL,
      baseUrl: '   ',
      enabled: true
    })

    expect(link).toEqual({ href: '/alarm/detail.htm?id=9849', isAbsolute: false })
  })

  it('should pass the url through, with a leading slash, when there is no datasource url to strip', () => {
    const link = resolveOpenNMSLink('alarm/detail.htm?id=7', {})

    expect(link).toEqual({ href: '/alarm/detail.htm?id=7', isAbsolute: false })
  })
})

describe('ClientDelegate :: getOpenNMSLink', () => {
  const settingsFor = (jsonData: object) => ({
    url: PROXY_URL,
    type: 'opennms-entity-datasource',
    name: 'opennms-entity-datasource',
    jsonData
  })

  it('should build an absolute link when the OpenNMS Base URL is enabled', () => {
    const client = new ClientDelegate(settingsFor({
      useOpenNMSBaseUrl: true,
      opennmsBaseUrl: 'http://localhost:8980/opennms'
    }), undefined)

    expect(client.getOpenNMSLink(DETAILS_PAGE)).toEqual({
      href: 'http://localhost:8980/opennms/alarm/detail.htm?id=9849',
      isAbsolute: true
    })
  })

  it('should build a relative link when the OpenNMS Base URL is not enabled', () => {
    const client = new ClientDelegate(settingsFor({
      useOpenNMSBaseUrl: false,
      opennmsBaseUrl: 'http://localhost:8980/opennms'
    }), undefined)

    expect(client.getOpenNMSLink(DETAILS_PAGE)).toEqual({
      href: '/alarm/detail.htm?id=9849',
      isAbsolute: false
    })
  })

  it('should build a relative link for a datasource saved before this setting existed', () => {
    const client = new ClientDelegate(settingsFor({}), undefined)

    expect(client.getOpenNMSLink(DETAILS_PAGE)).toEqual({
      href: '/alarm/detail.htm?id=9849',
      isAbsolute: false
    })
  })

  it('should return undefined when the alarm has no details page', () => {
    const client = new ClientDelegate(settingsFor({ useOpenNMSBaseUrl: true, opennmsBaseUrl: 'http://x/opennms' }), undefined)

    expect(client.getOpenNMSLink(undefined)).toBeUndefined()
  })
})
