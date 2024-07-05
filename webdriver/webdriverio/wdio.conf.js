const os = require('os')
const path = require('path')
const { spawn, spawnSync } = require('child_process')

// keep track of the `tauri-driver` child process
let tauriDriver;

exports.config = {
    logLevel: 'debug',
    // runner: 'local', // added this
    specs: ['./test/specs/**/*.js'],
    maxInstances: 1,
    capabilities: [
        {
            maxInstances: 1,
            'tauri:options': {
                application: '../../rust-workspace/target/release/learning-paths-tauri-react',
            },
        },
    ],
    reporters: ['spec'],
    framework: 'mocha',
    mochaOpts: {
        ui: 'bdd',
        timeout: 10 * 1000, // in millis
    },

    // ensure the rust project is built since we expect this binary to exist for the webdriver sessions
    onPrepare: () => {
        console.log("Rebuilding everything...");
        spawnSync('cargo',
            ['build', '--workspace', '--release'],
            { cwd: path.join(process.env.FLAKE_DIR, "rust-workspace") });
        spawnSync('npm',
            ['run', 'tauri', 'build'],
            { cwd: process.env.FLAKE_DIR })
    },

    // ensure we are running `tauri-driver` before the session starts so that we can proxy the webdriver requests
    beforeSession: (config, capabilities, specs) => {
        console.log('Running tests with the following configuration:');
        console.debug('Config:', config);
        console.debug('Capabilities:', capabilities);
        console.debug('Specs:', specs);
        tauriDriver = spawn(
            path.resolve(os.homedir(), '.cargo', 'bin', 'tauri-driver'),
            [],
            { stdio: [null, process.stdout, process.stderr] }
        )
        console.log("spawned");
    },

    // clean up the `tauri-driver` process we spawned at the start of the session
    afterSession: () => {
        console.log("clean up");
        tauriDriver.kill();
        console.log("cleaned up");
    }
}
