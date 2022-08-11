#!/usr/bin/env node

const os = require('os');
const fs = require('fs-extra');
const path = require('path');
const rimraf = require('rimraf');
const which = require('which');

const validator = require('specit/lib/validator');
const generate = require('specit/lib/generate');
const clean = require('specit/lib/clean');

const program = require('commander');

const spawn = require('child_process').spawnSync;
const cwd = process.cwd();

const pkginfo = require('./package.json');

let version = pkginfo.version;
let release = 1;

if (version.indexOf('-SNAPSHOT') > 0) {
  version = version.replace('-SNAPSHOT', '');
  release = '0';
}

try {
  which.sync('rpmbuild');
} catch (err) {
  console.log('rpmbuild executable not found');
  process.exit(1);
}

program
  .version(pkginfo.version)
  .option('-r --release <release>', 'Specify release number of package')
  .parse(process.argv);

const options = program.opts();
if (options.release === undefined) {
  options.release = release;
}

pkginfo.version = version;
pkginfo.release = release;
release = options.release;

console.log('Generating RPM spec for ' + pkginfo.name + ' ' + pkginfo.version + '-' + options.release);

clean('.', pkginfo);
generate(cwd, pkginfo, options, pkginfo.name, function (err, generated) {
  if (err) {
    console.error('Error:', err.message);
    process.exit(1);
  }

  generated.forEach(function (file) {
    console.log('Created ./%s', file);
  });

  const rpmbuilddir = path.join(os.tmpdir(), 'rpmbuild');
  rimraf.sync(rpmbuilddir);
  fs.mkdirSync(rpmbuilddir);
  fs.mkdirSync(path.join(rpmbuilddir, 'SOURCES'));
  fs.mkdirSync(path.join(rpmbuilddir, 'RPMS'));
  fs.mkdirSync(path.join(rpmbuilddir, 'BUILD'));
  fs.copySync(path.join('SOURCES', pkginfo.name + '.tar.gz'), path.join(rpmbuilddir, 'SOURCES', pkginfo.name + '.tar.gz'));

  console.log('Running rpmbuild');
  const ret = spawn(
    'rpmbuild',
    [
      '--define', '_topdir ' + path.join(os.tmpdir(), 'rpmbuild'),
      '-ba',
      'SPECS/opennms-helm.spec'
    ],
    {
      stdio: ['inherit', 'inherit', 'inherit']
    }
  );
  if (ret.error) {
    console.log('rpmbuild failed');
    process.exit(1);
  }

  const targetdir = path.join('artifacts');
  if (!fs.existsSync(targetdir)) {
    fs.mkdirSync(targetdir);
  }

  const rpm = pkginfo.name + '-' + version + '-' + release + '.noarch.rpm';
  if (fs.existsSync(path.join(targetdir, rpm))) {
    fs.unlinkSync(path.join(targetdir, rpm));
  }
  fs.copySync(path.join(rpmbuilddir, 'RPMS', 'noarch', rpm), path.join(targetdir, rpm));

  rimraf.sync(rpmbuilddir);

  process.exit(0);
});
