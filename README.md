## What this is

This is a small demo webapp written in Elm that interacts with the login management API at https://persona.api.ksfmedia.fi/v1/swagger-ui

## How to deploy

You must have Elm 0.19.0 installed. After cloning this repository and switching to its directory, do

    elm make ./src/Main.elm --optimize --output=main.js

You now have these files in the root directory:
  
- index.html
- main.js
- style.css

Now just host them together on any web server and navigate to `index.html`. 

To quickly test locally, run `elm reactor` and browse http://localhost:8000/index.html

## Other notes

All Elm code except for `src/Main.elm` was generated using _openapi-generator_. (Yes, ideally the generated code would be put into a separate package.)


