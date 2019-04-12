module.exports = function(grunt) {
  grunt.loadNpmTasks('grunt-exec');

  grunt.initConfig({

    clean: ["dist"],

    exec: {
      build: {
        command: 'yarn run build',
      },
      eslint: {
        command: 'yarn run eslint',
      },
      test: {
        command: 'yarn run test',
      },
      watch: {
        command: 'yarn run watch',
      }
    }
  });

  grunt.registerTask('build', [ 'exec:build' ]);
  grunt.registerTask('eslint', [ 'exec:eslint' ]);
  grunt.registerTask('test', [ 'exec:test' ]);
  grunt.registerTask('watch', [ 'exec:watch' ]);

  grunt.registerTask('default', [
    'clean',
    'eslint',
    'build',
    'test',
  ]);
};
