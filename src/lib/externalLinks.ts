/**
 * Helpers for building links out to a running OpenNMS instance.
 *
 * opennms-js builds urls such as Alarm.detailsPage from the datasource url, and whether that is
 * usable as a link target depends on the datasource's access mode:
 *
 * - Proxy access: the frontend url is Grafana's own '/api/datasources/proxy/uid/<uid>', and the
 *   configured url is one only the Grafana server need reach - typically a Docker
 *   'host.docker.internal' address. Neither reaches OpenNMS from the browser, so such links have
 *   to be re-rooted onto the base URL the user supplies via the "OpenNMS Base URL" setting.
 * - Direct (browser) access: the browser makes the API calls itself, so the datasource url is
 *   already browser-reachable and opennms-js produced a working link. It is left alone.
 */

export interface OpenNMSLink {
  href: string

  /**
   * True when href resolves to an OpenNMS instance on its own and can be presented as a link.
   * False when it is only an OpenNMS-relative path, which callers must not make clickable.
   */
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
 * Whether a url is one the browser can resolve on its own.
 *
 * A scheme is required: the browser reads 'localhost:8980/opennms' as the unknown scheme
 * 'localhost:', and 'onms.example.com/opennms' as a path relative to the Grafana page, so
 * neither reaches OpenNMS. sanitizeUrl passes both through unchanged.
 */
export const isAbsoluteHttpUrl = (url: string | undefined): boolean => {
  return /^https?:\/\//i.test((url ?? '').trim())
}

/**
 * Reduce a url built against the datasource url to a path relative to an OpenNMS instance,
 * e.g. '/alarm/detail.htm?id=9849'. The OpenNMS-relative portion comes from opennms-js, so the
 * page path never needs to be hardcoded here.
 */
export const toOpenNMSRelativeUrl = (url: string, datasourceUrl: string | undefined): string => {
  const prefix = normalizeBaseUrl(datasourceUrl)

  if (prefix && url.startsWith(prefix)) {
    const relative = url.slice(prefix.length)

    return relative.startsWith('/') ? relative : `/${relative}`
  }

  // The url did not come from this datasource, so there is no prefix of ours to remove. If it
  // already carries a scheme it stands on its own and must be left alone; prepending '/' would
  // turn it into '/http://host/...'.
  if (isAbsoluteHttpUrl(url)) {
    return url.trim()
  }

  return url.startsWith('/') ? url : `/${url}`
}

/**
 * Resolve a url built against the datasource url into a link for display.
 *
 * Returns an absolute link when the user has enabled and entered an OpenNMS Base URL, and
 * otherwise an OpenNMS-relative one, which callers should not present as clickable.
 * Returns undefined when there is no url to resolve.
 */
export const resolveOpenNMSLink = (url: string | undefined, options: OpenNMSLinkOptions): OpenNMSLink | undefined => {
  const trimmed = url?.trim()

  if (!trimmed) {
    return undefined
  }

  const relative = toOpenNMSRelativeUrl(trimmed, options.datasourceUrl)

  // A url that could not be reduced to a path did not come from this datasource; it already
  // resolves on its own, so it is returned as it stands rather than appended to the base url.
  if (isAbsoluteHttpUrl(relative)) {
    return { href: relative, isAbsolute: true }
  }

  const baseUrl = normalizeBaseUrl(options.baseUrl)

  // A base url without a scheme cannot resolve, so it is ignored rather than used to build a
  // link that leads nowhere. The config editor reports it, but a datasource can also be
  // provisioned from YAML without ever passing through that editor.
  if (options.enabled && isAbsoluteHttpUrl(baseUrl)) {
    return { href: `${baseUrl}${relative}`, isAbsolute: true }
  }

  // Direct (browser) access mode: the datasource url is browser-reachable, so opennms-js
  // already built a working link and there is nothing to re-root.
  if (isAbsoluteHttpUrl(trimmed)) {
    return { href: trimmed, isAbsolute: true }
  }

  return { href: relative, isAbsolute: false }
}
