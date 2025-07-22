# build_remote.ps1
# Script para compilar programas COBOL contra build server remoto

param(
    [Parameter(Mandatory=$true)]
    [string]$ProgramName,
    
    [string]$BuildServer = "10.0.6.54",
    [string]$SourcePath = "/_env/cloud1st/DV/src",
    [string]$ToolsPath = "/_env/cloud1st/0-tools",
    [string]$Environment = "DV"
)

Write-Host "=== Build Remoto para $ProgramName ===" -ForegroundColor Green

# 1. Crear directorio local para resultados
if (!(Test-Path "compiled")) {
    New-Item -ItemType Directory -Name "compiled"
}

# 2. Subir archivos fuente
Write-Host "Subiendo archivos..." -ForegroundColor Yellow
scp "$ProgramName.cbl" "$BuildServer`:$SourcePath/"
if ($LASTEXITCODE -ne 0) {
    Write-Error "Error subiendo archivo principal $ProgramName.cbl"
    exit 1
}

# 3. Asignar permisos completos al archivo subido
Write-Host "Asignando permisos al archivo..." -ForegroundColor Yellow
ssh $BuildServer "chmod 777 $SourcePath/$ProgramName.cbl" 2>&1 | Out-Null
if ($LASTEXITCODE -eq 0) {
    Write-Host "Permisos asignados a $ProgramName.cbl" -ForegroundColor Green
} else {
    Write-Host "Error asignando permisos a $ProgramName.cbl" -ForegroundColor Yellow
}

# 4. Ejecutar compilación remota
Write-Host "Compilando en servidor remoto..." -ForegroundColor Yellow

# 5. Crear archivo de log local
$logFile = "compiled\$ProgramName-build.log"
$timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"

# 6. Cabecera del log
"=== Build Log para $ProgramName ===" | Out-File -FilePath $logFile -Encoding UTF8
"Timestamp: $timestamp" | Out-File -FilePath $logFile -Append -Encoding UTF8
"Servidor: $BuildServer" | Out-File -FilePath $logFile -Append -Encoding UTF8
"Ruta: $SourcePath" | Out-File -FilePath $logFile -Append -Encoding UTF8
"Entorno: $Environment" | Out-File -FilePath $logFile -Append -Encoding UTF8
"" | Out-File -FilePath $logFile -Append -Encoding UTF8

# 7. Ejecutar y capturar toda la salida
Write-Host "Ejecutando: bash $ToolsPath/compile.sh $Environment $ProgramName" -ForegroundColor Cyan
$buildCommand = "cd $ToolsPath; source /opt/microfocus/EnterpriseDeveloper/bin/cobsetenv;echo 'Compilando $ProgramName en entorno $Environment'; bash $ToolsPath/compile.sh $Environment $ProgramName; echo 'Compilacion completada'"

# Se usa Start-Process para evitar problemas con PowerShell y SSH
$psi = New-Object System.Diagnostics.ProcessStartInfo
$psi.FileName = "ssh"
$psi.Arguments = "$BuildServer `"$buildCommand`""
$psi.RedirectStandardOutput = $true
$psi.RedirectStandardError = $true
$psi.UseShellExecute = $false
$psi.CreateNoWindow = $true

$process = New-Object System.Diagnostics.Process
$process.StartInfo = $psi

try {
    $process.Start() | Out-Null
    $stdout = $process.StandardOutput.ReadToEnd()
    $stderr = $process.StandardError.ReadToEnd()
    $process.WaitForExit()
    $compileResult = $process.ExitCode
    
    # Combinar stdout y stderr
    $buildOutput = @()
    if ($stdout) { $buildOutput += $stdout -split "`n" }
    if ($stderr) { $buildOutput += $stderr -split "`n" }
} finally {
    if ($process) { $process.Dispose() }
}

# 8. Escribir la salida al log
"=== Salida del comando de compilación ===" | Out-File -FilePath $logFile -Append -Encoding UTF8
$buildOutput | Out-File -FilePath $logFile -Append -Encoding UTF8
"" | Out-File -FilePath $logFile -Append -Encoding UTF8
"Código de salida: $compileResult" | Out-File -FilePath $logFile -Append -Encoding UTF8

# Filtrar mensajes de banner de SSH y errores de PowerShell de manera segura
$filteredOutput = $buildOutput | Where-Object { 
    # Usar -notlike en lugar de -notmatch para evitar problemas con regex
    $line = $_ -as [string]
    $line -notlike "*This system is the property*" -and 
    $line -notlike "*Unauthorized access*" -and 
    $line -notlike "*violation*Policies*" -and
    $line -notlike "*Use of this system*" -and
    $line -notlike "*financial penalties*" -and
    $line -notlike "*legal action*" -and
    $line -notlike "*At C:\Users*" -and
    $line -notlike "*CategoryInfo*" -and
    $line -notlike "*FullyQualifiedErrorId*" -and
    $line -notlike "*consent to monitoring*" -and
    $line -notlike "*accordance with*Policies*" -and
    $line -ne "" -and
    $line.Trim() -ne ""
} | ForEach-Object {
    # Limpiar códigos de escape ANSI (colores del terminal)
    $cleanLine = $_ -replace '\x1b\[[0-9;]*m', ''
    $cleanLine.Trim()
} | Where-Object { $_ -ne "" }

$filteredOutput | ForEach-Object { 
    # Usar -like para patrones más seguros
    if ($_ -like "*error*" -or $_ -like "*Error*" -or $_ -like "*ERROR*" -or $_ -like "*compilation return code: [1-9]*") {
        Write-Host $_ -ForegroundColor Red
    } elseif ($_ -like "*warning*" -or $_ -like "*Warning*" -or $_ -like "*WARNING*") {
        Write-Host $_ -ForegroundColor Yellow
    } else {
        Write-Host $_
    }
}

if ($compileResult -ne 0) {
    Write-Host "Error en compilación remota (código: $compileResult)" -ForegroundColor Red
    
    # Intentar descargar el archivo .lst para ver los errores
    Write-Host "Descargando archivo de log del compilador..." -ForegroundColor Yellow
    scp "$BuildServer`:$SourcePath/$ProgramName.lst" "compiled/" 2>$null
    
    if (Test-Path "compiled/$ProgramName.lst") {
        Write-Host "=== Contenido del archivo .lst ===" -ForegroundColor Yellow
        $lstContent = Get-Content "compiled/$ProgramName.lst" -Encoding UTF8
        $lstContent | ForEach-Object {
            if ($_ -like "*error*" -or $_ -like "*Error*" -or $_ -like "*ERROR*" -or $_ -like "**") {
                Write-Host $_ -ForegroundColor Red
            } elseif ($_ -like "*warning*" -or $_ -like "*Warning*" -or $_ -like "*WARNING*") {
                Write-Host $_ -ForegroundColor Yellow
            } else {
                Write-Host $_
            }
        }
        
        # Agregar contenido del .lst al log
        "" | Out-File -FilePath $logFile -Append -Encoding UTF8
        "=== Contenido del archivo .lst ===" | Out-File -FilePath $logFile -Append -Encoding UTF8
        $lstContent | Out-File -FilePath $logFile -Append -Encoding UTF8
    } else {
        Write-Host "No se pudo descargar el archivo .lst" -ForegroundColor Red
        
        # Intentar ver el contenido directamente en el servidor
        Write-Host "Intentando leer .lst directamente del servidor..." -ForegroundColor Yellow
        $remoteLst = ssh $BuildServer "cat $SourcePath/$ProgramName.lst 2>/dev/null || echo 'Archivo .lst no encontrado'"
        if ($remoteLst) {
            Write-Host "=== Contenido remoto del .lst ===" -ForegroundColor Yellow
            $remoteLst | ForEach-Object {
                if ($_ -like "*error*" -or $_ -like "*Error*" -or $_ -like "*ERROR*" -or $_ -like "**") {
                    Write-Host $_ -ForegroundColor Red
                } elseif ($_ -like "*warning*" -or $_ -like "*Warning*" -or $_ -like "*WARNING*") {
                    Write-Host $_ -ForegroundColor Yellow
                } else {
                    Write-Host $_
                }
            }
            
            # Agregar al log
            "" | Out-File -FilePath $logFile -Append -Encoding UTF8
            "=== Contenido remoto del .lst ===" | Out-File -FilePath $logFile -Append -Encoding UTF8
            $remoteLst | Out-File -FilePath $logFile -Append -Encoding UTF8
        }
    }
    
    Write-Host "Log completo guardado en: $logFile" -ForegroundColor Magenta
    exit 1
}

Write-Host "Log guardado en: $logFile" -ForegroundColor Green

# 9. Descargar resultados
Write-Host "Descargando resultados..." -ForegroundColor Yellow
scp "$BuildServer`:$SourcePath/$ProgramName.int" "compiled/" 2>$null
scp "$BuildServer`:$SourcePath/$ProgramName.idy" "compiled/" 2>$null
scp "$BuildServer`:$SourcePath/$ProgramName.lst" "compiled/" 2>$null

# 10. Mostrar resultados
Write-Host "=== Build Completado ===" -ForegroundColor Green
Get-ChildItem "compiled/$ProgramName.*" | ForEach-Object {
    Write-Host "✅ $($_.Name)" -ForegroundColor Cyan
}

Write-Host "Para ejecutar: cob compiled/$ProgramName.int" -ForegroundColor Magenta
