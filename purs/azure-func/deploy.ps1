param (
  $FuncName = "mea-func-test", 
  $ResourceGroupName = "Automation")

$MkDeployment = $PSScriptRoot + "\mk-deployment.ps1"
$deployment = $PSScriptRoot + "\deploy.zip"

echo ("Writing deployment zip: " + $deployment)
invoke-expression -command $MkDeployment

echo "Publishing..."
Publish-AzWebApp -ResourceGroupName $ResourceGroupName -Name $FuncName -ArchivePath $deployment

echo "Cleaning up."
rm $deployment

