module.exports = function(grunt) {

  require('load-grunt-tasks')(grunt);

  grunt.loadNpmTasks('grunt-execute');
  grunt.loadNpmTasks('grunt-contrib-clean');

  grunt.initConfig({

    clean: ["dist", "vendor/opennms.js"],

    copy: {
      src_to_dist: {
        cwd: 'src',
        expand: true,
        src: ['**/*', '!**/*.js', '!**/*.scss'],
        dest: 'dist'
      },
      opennms_to_vendor: {
        src: 'node_modules/opennms/dist/opennms.min.js',
        dest: 'vendor/opennms.js'
      },
      opennms_map_to_vendor: {
        src: 'node_modules/opennms/dist/opennms.min.js.map',
        dest: 'vendor/opennms.js.map'
      },
      vendor_to_dist: {
        cwd: 'vendor',
        expand: true,
        src: ['**/*'],
        dest: 'dist'
      },
      vendor_to_dist_tests: {
        cwd: 'vendor',
        expand: true,
        src: ['**/*'],
        dest: 'dist/test'
      },
      pluginDef: {
        expand: true,
        src: ['README.md'],
        dest: 'dist',
      }
    },

    watch: {
      rebuild_all: {
        files: ['src/**/*', 'vendor/**/*', 'README.md'],
        tasks: ['default'],
        options: {spawn: false}
      },
    },

    babel: {
      options: {
        sourceMap: true,
        presets:  ["es2015"]
      },
      dist: {
        options: {
          plugins: ['transform-es2015-modules-systemjs', 'transform-es2015-for-of']
        },
        files: [{
          cwd: 'src',
          expand: true,
          src: ['**/*.js'],
          dest: 'dist',
          ext:'.js'
        }]
      },
      distTestNoSystemJs: {
        files: [{
          cwd: 'src',
          expand: true,
          src: ['**/*.js'],
          dest: 'dist/test',
          ext:'.js'
        }]
      },
      distTestsSpecsNoSystemJs: {
        files: [{
          expand: true,
          cwd: 'src/spec',
          src: ['**/*.js'],
          dest: 'dist/test/spec',
          ext:'.js'
        }]
      },
    },

    sass: {
      options: {
        sourceMap: true
      },
      dist: {
        files: {
          "dist/panels/alarm-table/css/table.dark.css": "src/panels/alarm-table/sass/table.dark.scss",
          "dist/panels/alarm-table/css/table.light.css": "src/panels/alarm-table/sass/table.light.scss",
          "dist/datasources/perf-ds/css/opennms.dark.css": "src/datasources/perf-ds/sass/opennms.dark.scss",
          "dist/datasources/perf-ds/css/opennms.light.css": "src/datasources/perf-ds/sass/opennms.light.scss"
        }
      }
    },

    mochaTest: {
      test: {
        options: {
          reporter: 'spec'
        },
        src: ['dist/test/spec/test-main.js', 'dist/test/spec/*_spec.js']
      }
    }
  });

  grunt.registerTask('default', [
    'clean',
    'copy',
    'babel',
    'sass',
    'mochaTest'
  ]);
};
