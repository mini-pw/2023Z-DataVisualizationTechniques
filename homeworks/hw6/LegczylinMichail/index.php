<?php
require_once('jpgraph-4.4.1/src/jpgraph.php');
require_once('jpgraph-4.4.1/src/jpgraph_line.php');

// Some data
$dataX = [];
$dataY = [];

if (($handle = fopen("data.csv", "r")) !== FALSE) {
    for ($row = 1; ($dataFromLine = fgetcsv($handle)) !== FALSE; $row++) {
        $slice = array_slice($dataFromLine, 2);
        // first row is column names. ignore
        if ($row === 1) {
            $dataX = $slice;

            continue;
        }

        $dataY[$dataFromLine[0]] = $slice;
    }

    fclose($handle);
}

// Setup graph
$graph = new Graph(1270, 720, "auto");
$graph->img->SetMargin(150, 150, 40, 120);
$graph->SetScale("textlin");
$graph->SetShadow();

// Setup title
$graph->title->Set("Selected countries' populations in 1960 - 2021 years");

// Adjust legend
$graph->legend->Pos(0.5, 0.95, "center", "center");

foreach ($dataY as $key => $value) {
    if (!in_array($key, ["Ukraine", "Poland", "Norway", "Sweden", "Germany"])) continue;

    $plot = new LinePlot($value);
    $plot->SetLegend($key);

    $plot->mark->SetType(MARK_CIRCLE);
    $plot->mark->SetFillColor("red");
    $plot->mark->SetWidth(3);

    $graph->Add($plot);
}

// Setup X-scale
$graph->xaxis->SetTickLabels($dataX);
$graph->xaxis->SetLabelAngle(45);
$graph->xaxis->SetTextTickInterval(3,1);

// Output
$graph->Stroke();