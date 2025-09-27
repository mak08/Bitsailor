var colors = [];

function setupColors () {
    colors.push("#0080ff");
    colors.push("#00ffff");
    colors.push("#00ffa0");
    colors.push("#00cc00");
    colors.push("#a0ff00");
    colors.push("#ffff00");
    colors.push("#ffc000");
    colors.push("#ff8000");
    colors.push("#ff0000");
    colors.push("#f00080");
    colors.push("#d020ff");
    colors.push("#8020ff");
    colors.push("#0000ff");
}

function ms2bf ( windspeed ) {
    // Beaufort scale according to http://de.wikipedia.org/wiki/Beaufortskala
    if ( windspeed < 0.3 ) {
        return 0;
    } else if ( windspeed < 1.6 ) {
        return 1;
    } else if ( windspeed < 3.4 ) {
        return 2;
    } else if ( windspeed < 5.5 ) {
        return 3;
    } else if ( windspeed < 8.0 ) { 
        return 4;
    } else if ( windspeed < 10.8 ) {
        return 5;
    } else if ( windspeed < 13.9 ) {
        return 6;
    } else if ( windspeed < 17.2 ) {
        return 7;
    } else if ( windspeed < 20.8 ) {
        return 8;
    } else if ( windspeed < 24.5 ) {
        return 9;
    } else if ( windspeed < 28.5 ) {
        return 10;
    } else if ( windspeed < 32.7 ) {
        return 11;
    } else {
        return 12;
    }
}

setupColors();

export {
    colors,
    ms2bf
}
