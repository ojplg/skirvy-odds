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

        options: {
            legend: { 
                display: false
            },
            scales: {
                yAxes: [{
                    ticks: {
                        beginAtZero:true
                    }
                }]
            }
        }
    });
    console.log("Charted!");
}
