<?php
$subject_id = $_GET['PROLIFIC_PID'];
$study_id = $_GET['STUDY_ID'];
$session_id = $_GET['SESSION_ID'];
$id_filename = '../../data/participant_id.txt';
$destination = "../index.html?PROLIFIC_PID=".$subject_id."&STUDY_ID=".$study_id."&SESSION_ID=".$session_id."&participant=";

$last_id = file_get_contents($id_filename);
if ($last_id === FALSE) {
  $id = 0;
} else {
  $id = (int)$last_id;
}
$id += 1;
file_put_contents($id_filename, $id);
header('Location: '.$destination.$id);
?>