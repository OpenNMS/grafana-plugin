# Development Notes

## OPG Version: 12

A place to put various notes that may help in development, or discuss odd behaviors.


## swc/core

Seems to be some errors with grafana libraries and `@swc/core`, you may get `Failed to load native bindings` or similar errors.

Solution for now is to force use of `@swc/core` version `1.3.75`. It seems some incompatibility was introduced in `@swc/core` `1.3.76`.

Should monitor this to see if a solution has been found so we can bump up our `@swc/core` version.

See:

https://community.grafana.com/t/build-a-panel-plugin-error/100984/3



## makerpm.js

This is a script that uses `bbc/speculate` to create RPM `spec` files, and then create an RPM.

We previously used the `specit` library, but it hasn't been maintained since 2016 or so, and some dependencies were out of date and had security issues.

`specit` was a fork of `bbc/speculate`, so we decided to use that instead, with some updates. Another option might be to fork `specit` and just update its libraries.

See https://github.com/bbc/speculate for more info.

Note that the `spec` section in the `package.json` contains options for `speculate`.

There's a `const isDebug = false` in the `makerpm.js` code, you can set it to true if you want some additional debug log output, useful for debugging issues
when running in CircleCI.

Note, we only want the files in `dist` to be part of the RPM package; for example we do *not* want `node_modules` to be included. So we pass the root-level `package.json` but then tell `speculate` that the root directory is `dist`. `speculate` creates files under `dist/SPECS` and `dist/SOURCES`, we then copy those back to the main root directory for `rpmbuild` to work correctly and for the artifacts to be in the right place.

`speculate` will include `node_modules` by default (see [archiver.js](https://github.com/bbc/speculate/blob/master/lib/archiver.js), `REQUIRED_ENTRIES`), but `specit` does not, so we have to do this extra hacky step.


## grafana/plugin-validator

This is run in `.circleci/config.yml`, `validate-packages` step. `@grafana/plugin-validator` is a package from Grafana that validates plugins.

We have to pass this validation in order for Grafana to accept our plugin and put it on their app store. They may have additional validation, but this at least helps us comply.

The `-config` argument points to a yaml configuration file. One main thing is the `osv-scanner` which runs security vulnerability checks, basically checking everything
in our `package-lock.json` to see if there are CVEs, etc.

If we are temporarily non-compliant but you are trying to just get a build done in CircleCI, you can update `grafana-plugin-validator-config.yaml` as follows:

```
analyzers:
  osv-scanner:
    enabled: false
```

**Make sure** to set this back to `true` before actual production builds.

## osv-scanner

This is a tool that Grafana will run to see if we have any npm libraries with security vulnerabilities, etc. in our Grafana plugin code.

You can run this locally. On a Mac:

```
brew install osv-scanner

# from your main grafana-plugins directory
osv-scanner -L package-lock.json
```

This outputs a table with any possible issues.

If there are any libraries that have something in `FIXED VERSION`, you'll need to make sure to update, include transient dependencies.

## transient dependencies

You may be able to fix some transient dependencies, i.e. some libraries failing the `osv-scanner` but aren't direct dependencies.

Use the `npm overrides` mechanism in the `package.json`. Delete the `package-lock.json` and rerun `npm install`.

```
"overrides": {
  "opennms": {
    "striptags": "^3.2.0"
  },
  "html-to-formatted-text": {
    "striptags": "^3.2.0"
  }
}
```

## opennms-js and json-source-map isJSON issue

This is described more fully here: https://github.com/OpenNMS/opennms-js/pull/1118

Just note that if `opennms-js` has any model data classes which have a `toJSON` method (which returns a somewhat more human-readable version of the object), it will also have this fix, meaning the object will also have a fake `.replace()` method on it. Should not cause any issues, but just noting it here.
