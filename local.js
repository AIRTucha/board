// load Elm module
const app = require('./dist/main.js').Port.worker()

setInterval( () => {
    app.ports.suggestions.send("test")
}, 100 )