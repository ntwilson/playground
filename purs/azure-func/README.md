Follows the instructions from https://medium.com/lambda-factory/purescript-on-azure-functions-e75d64b22165 to create an azure function using PureScript.

### Setup
```bash
npm install
npx spago build
```

### Run locally
```bash
npx func host start
```
then navigate to the URL given in the output log 

### Deploy
*prerequisites*
install Azure PowerShell.  (https://docs.microsoft.com/en-us/powershell/azure/install-az-ps?view=azps-4.2.0) (note: read to the bottom for instructions on signing in)

```bash
.\deploy.ps1
```

This assumes the default values for this particular test function, which is in ResourceGroup "Automation" and Azure Function App "mea-func-test".  One or both of these values can be set when calling the deploy script

```bash
.\deploy.ps1 -FuncName "mea-func-test" -ResourceGroupName "Automation"
```


