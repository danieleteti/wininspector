# WinInspector - Advanced Windows Analysis Tool
WinInspector is a real-time Windows inspection utility that provides comprehensive information about windows, processes, and monitors under your mouse cursor. Born as the spiritual successor to TDWinInfo (originally developed 20+ years ago), WinInspector offers significantly enhanced functionality and modern capabilities.

Read the full blog post about [WinInspector](https://www.danieleteti.it/post/wininspector-the-tdwininfo-legacy-continues/).

![](https://raw.githubusercontent.com/danieleteti/wininspector/refs/heads/main/docs/wininspector_sample01.png)

🚀 Key Features
- Real-time window inspection - Live updates every 500ms as you move your mouse
- Comprehensive window details - Handle, class name, title, dimensions, visibility, styles
- Process information - PID, TID, executable name and path
- Window hierarchy - Parent and root window relationships
- **🎨 VCL Component Inspector** - Detects Delphi/C++Builder VCL components and extracts properties (Caption, Enabled, Checked, ItemCount, etc.)
- **📚 DLL/Library Inspector** - Lists all loaded DLLs and BPL packages, identifies Delphi version (XE2 to Delphi 13)
- Monitor information - Multi-display support with resolution and work area details
- Window responsiveness - Detects if windows are responding to messages
- Tabbed interface - Organized view with separate tabs for Window Info and DLL/Libraries
- Clean, professional UI - No configuration needed, just run and inspect

🔧 Technical Details
- Built with Delphi and VCL
- Uses Windows API for low-level system access
- Lightweight and portable
- No installation required

🎯 Perfect for
- **Delphi/C++Builder developers** - Debug VCL components, identify Delphi versions, troubleshoot deployment
- Windows developers debugging UI issues
- System administrators analyzing window problems and DLL dependencies
- QA engineers testing window behavior and component states
- Anyone curious about Windows internals and Delphi applications

WinInspector is a modern evolution of the classic TDWinInfo tool with enhanced capabilities for today's multi-monitor, complex Windows environments.
