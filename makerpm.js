#!/usr/bin/env node

var os = require('os');
var fs = require('fs-extra');
var path = require('path');
var rimraf = require('rimraf');
var which = require('which');

var validator = require('specit/lib/validator');
var generate = require('specit/lib/generate');
var clean = require('specit/lib/clean');

var program = require('commander');

var spawn = require('child_process').spawnSync;
var cwd = process.cwd();

var pkginfo = require('./package.json');

var version = pkginfo.version;
var release = 1;

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

if (program.release === undefined) {
  program.release = release;
}

pkginfo.version = version;
pkginfo.release = release;
release = program.release;

console.log('Generating RPM spec for ' + pkginfo.name + ' ' + pkginfo.version + '-' + program.release);

clean('.', pkginfo);
generate(cwd, pkginfo, program, pkginfo.name, function (err, generated) {
  if (err) {
    console.error('Error:', err.message);
    process.exit(1);
  }

  generated.forEach(function (file) {
    console.log('Created ./%s', file);
  });

  var rpmbuilddir = path.join(os.tmpdir(), 'rpmbuild');
  rimraf.sync(rpmbuilddir);
  fs.mkdirSync(rpmbuilddir);
  fs.mkdirSync(path.join(rpmbuilddir, 'SOURCES'));
  fs.mkdirSync(path.join(rpmbuilddir, 'RPMS'));
  fs.mkdirSync(path.join(rpmbuilddir, 'BUILD'));
  fs.copySync(path.join('SOURCES', pkginfo.name + '.tar.gz'), path.join(rpmbuilddir, 'SOURCES', pkginfo.name + '.tar.gz'));

  console.log('Running rpmbuild');
  var ret = spawn(
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

  var targetdir = path.join('dist', 'packages');
  if (!fs.existsSync(targetdir)) {
    fs.mkdirSync(targetdir);
  }

  var rpm = pkginfo.name + '-' + version + '-' + release + '.noarch.rpm';
  if (fs.existsSync(path.join(targetdir, rpm))) {
    fs.unlinkSync(path.join(targetdir, rpm));
  }
  fs.copySync(path.join(rpmbuilddir, 'RPMS', 'noarch', rpm), path.join(targetdir, rpm));

  rimraf.sync(rpmbuilddir);

  process.exit(0);
});
