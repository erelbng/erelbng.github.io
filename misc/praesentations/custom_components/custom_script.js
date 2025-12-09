// custom_script.js

window.addEventListener("load", function() {
    // Get header HTML (either from inline div or dynamically loaded)
    const header = $('#header').html();

    // Only proceed if the header actually exists
    if (!header) {
        console.warn("Header not found — skipping Reveal header/footer setup.");
        return;
    }

    // Reveal.js 'ready' event ensures all slides are in the DOM
    Reveal.addEventListener('ready', function(event) {
        if (window.location.search.match(/print-pdf/gi)) {
            // For print-PDF mode
            $('.slide-background').append(header);
            console.log("Header/footer added to PDF slides.");
        } else {
            // For normal presentation mode
            $('div.reveal').append(header);
            console.log("Header/footer added to live Reveal presentation.");
        }
    });
});
