function drawWind (windCanvas, xSteps, ySteps, data) {
    var windData = data[3];
    var ctx = windCanvas.getContext("2d");
    ctx.globalAlpha = 0.6;
    ctx.clearRect(0, 0, windCanvas.width, windCanvas.height);

    var dx =  windCanvas.width/xSteps;
    var dy =  windCanvas.height/ySteps;
    
    for ( var y = 0; y < ySteps; y++ ) {
        var yOffset = y * dy + (dy / 2);
        for ( var x = 0; x < xSteps; x++ ) {
            var xOffset = x * dx + (dx / 2);
            drawWindArrow(ctx, xOffset, yOffset, windData[y][x][0], windData[y][x][1]);
        }
    }
}

function drawWindArrow(ctx, x, y, direction, speed) {
    direction = direction + 90;
    if (direction > 360) {
        direction = direction - 360;
    } 
    ctx.fillStyle = colors[ms2bf(speed)];
    ctx.strokeStyle = colors[ms2bf(speed)];
    ctx.save();
    ctx.translate(x, y);
    ctx.rotate((direction*Math.PI/180));
    var scale = (speed>0)?0.4 + speed/30:0;
    ctx.scale(scale, scale);
    ctx.beginPath();
    ctx.moveTo(-0, 0);
    ctx.lineTo(-15, 12);
    ctx.lineTo(18, 0);
    ctx.lineTo(-15, -12);
    ctx.closePath()
    ctx.fill();
    ctx.stroke();
    ctx.restore();
}

var colors = ["#0080ff", "#00ffff", "#00ffa0", "#00cc00", "#a0ff00", "#ffff00", "#ffc000", "#ff8000", "#ff0000", "#f00080", "#d020ff", "#8020ff", "#0000ff"];

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
