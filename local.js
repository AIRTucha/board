// load Elm module
const app = require('./dist/main.js').App.worker()

setInterval( () => {
    app.ports.suggestions.send("test")
}, 100 )