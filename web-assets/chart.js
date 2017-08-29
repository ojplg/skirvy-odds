function drawChart() {
    console.log("Charting!");
    var ctx = document.getElementById("chart").getContext('2d');
    var resultsChart = new Chart(ctx, {
        type: 'bar',
        data: {
            labels: histAxis,
            datasets: [{
                data: histValues,
                backgroundColor: colorValues
            }]
        },

        options: {}
    });
    console.log("Charted!");
}
