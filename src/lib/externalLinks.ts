/**
 * Helpers for building links out to a running OpenNMS instance.
 *
 * The URL configured in a datasource's HTTP settings is not usable as a link target: in proxy
 * access mode Grafana rewrites it to '/api/datasources/proxy/uid/<uid>' on the frontend, and in
 * direct mode it may be an address only the Grafana server can reach (a Docker
 * 'host.docker.internal' address, for example). opennms-js builds Alarm.detailsPage from that url,
 * so it has to be re-rooted onto a base URL the user's browser can reach, which the user supplies
 * separately via the "OpenNMS Base URL" datasource setting.
 */

export interface OpenNMSLink {
  href: string

  /** True when href points at a configured OpenNMS instance, false when it is only a relative path. */
  isAbsolute: boolean
}

export interface OpenNMSLinkOptions {
  /** The datasource url as seen by the frontend, used as the prefix to strip. */
  datasourceUrl?: string

  /** User-configured base URL of the OpenNMS instance, as reachable from the browser. */
  baseUrl?: string

  /** Whether the user has enabled the OpenNMS Base URL setting. */
  enabled?: boolean
}

/** Trim whitespace and any trailing slashes, so a relative path can be appended directly. */
export const normalizeBaseUrl = (baseUrl: string | undefined): string => {
  return (baseUrl ?? '').trim().replace(/\/+$/, '')
}

/**
 * Reduce a url built against the datasource url to a path relative to an OpenNMS instance,
 * e.g. '/alarm/detail.htm?id=9849'. The OpenNMS-relative portion comes from opennms-js, so the
 * page path never needs to be hardcoded here.
 */
export const toOpenNMSRelativeUrl = (url: string, datasourceUrl: string | undefined): string => {
  const prefix = normalizeBaseUrl(datasourceUrl)
  const relative = prefix && url.startsWith(prefix) ? url.slice(prefix.length) : url

  return relative.startsWith('/') ? relative : `/${relative}`
}

/**
 * Resolve a url built against the datasource url into a link for display.
 *
 * Returns an absolute link when the user has enabled and entered an OpenNMS Base URL, and
 * otherwise an OpenNMS-relative one, which callers should not present as clickable.
 * Returns undefined when there is no url to resolve.
 */
export const resolveOpenNMSLink = (url: string | undefined, options: OpenNMSLinkOptions): OpenNMSLink | undefined => {
  if (!url?.trim()) {
    return undefined
  }

  const relative = toOpenNMSRelativeUrl(url.trim(), options.datasourceUrl)
  const baseUrl = normalizeBaseUrl(options.baseUrl)

  if (options.enabled && baseUrl) {
    return { href: `${baseUrl}${relative}`, isAbsolute: true }
  }

  return { href: relative, isAbsolute: false }
}
