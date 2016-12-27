const express     = require('express')
const wsss        = require('wsss')

const sharedState = wsss(8000, {
    info: console.log
})

const app = express()
app.use(express.static('public'))
console.log('Serving files under ./public/ on port 3000')
app.listen(3000)
