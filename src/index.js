import { Main } from './Main.elm';
import registerServiceWorker from './js/registerServiceWorker';
import './js/elm-pep';
import './js/drift';

var app = Main.fullscreen(localStorage.session || null);

app.ports.storeSession.subscribe(function (session) {
    localStorage.session = session;
});

window.addEventListener("storage", function (event) {
    if (event.storageArea === localStorage && event.key === "session") {
        app.ports.onSessionChange.send(event.newValue);
    }
}, false);

app.ports.fileSelected.subscribe(function (id) {
    var node = document.getElementById(id);

    if (node === null) {
        return;
    }

    // If your file upload field allows multiple files, you might
    // want to consider turning this into a `for` loop.
    for (let i = 0; i < node.files.length; i++) {
        const file = node.files[i];

        var reader = new FileReader();

        // FileReader API is event based. Once a file is selected
        // it fires events. We hook into the `onload` event for our reader.
        reader.onload = (function (event) {
            // The event carries the `target`. The `target` is the file
            // that was selected. The result is base64 encoded contents of the file.
            var base64encoded = event.target.result;
            // We build up the `ImagePortData` object here that will be passed to our Elm
            // runtime through the `fileContentRead` subscription.
            var portData = {
                content: base64encoded,
                filename: file.name,
            };

            // We call the `fileContentRead` port with the file data
            // which will be sent to our Elm runtime via Subscriptions.
            console.log(portData);
            app.ports.fileContentRead.send(portData);
        });

        // Connect our FileReader with the file that was selected in our `input` node.
        reader.readAsDataURL(file);
    }
});

registerServiceWorker();
