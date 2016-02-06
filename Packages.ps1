C:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe Persimmon.Dried.sln /property:Configuration=Release /property:VisualStudioVersion=12.0 /target:rebuild

.\.nuget\nuget.exe pack .\src\Persimmon.Dried\Persimmon.Dried.fsproj -Symbols -Properties VisualStudioVersion=12.0 -Prop Configuration=Release
.\.nuget\nuget.exe pack .\src\Persimmon.Dried.Ext\Persimmon.Dried.Ext.fsproj -Symbols -Properties VisualStudioVersion=12.0 -Prop Configuration=Release

if(Test-Path "nuget-packages")
{
  rm nuget-packages -Recurse -Force
}
mkdir nuget-packages

mv .\*.nupkg nuget-packages

ls .\nuget-packages\*.nupkg | ?{
  -not $_.Name.Contains('.symbols.')
} | %{
  echo "..\.nuget\nuget.exe push $_" >> .\nuget-packages\Push-All.ps1
}
