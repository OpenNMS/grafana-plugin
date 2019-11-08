module.exports = function(grunt) {
  grunt.loadNpmTasks('grunt-exec');

  grunt.initConfig({

    clean: ["dist"],

    exec: {
      build: {
        command: 'npm run build',
      },
      eslint: {
        command: 'npm run eslint',
      },
      test: {
        command: 'npm run test',
      },
      watch: {
        command: 'npm run watch',
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
