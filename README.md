# WinInspector 2.1 - Advanced Windows Analysis Tool
WinInspector is a real-time Windows inspection utility that provides comprehensive information about windows, processes, and monitors under your mouse cursor. Born as the spiritual successor to TDWinInfo (originally developed 20+ years ago), WinInspector offers significantly enhanced functionality and modern capabilities.

📖 Read the full announcement: [WinInspector 2.1 Release](https://www.danieleteti.it/post/wininspector-release-2-1-en/) | [Original Release](https://www.danieleteti.it/post/wininspector-the-tdwininfo-legacy-continues/)

![](https://raw.githubusercontent.com/danieleteti/wininspector/refs/heads/main/docs/wininspector_2_1.jpg)

🚀 Key Features
- **Real-time window inspection** - Live updates every 500ms as you move your mouse
- **Complete window hierarchy** - Full parent chain from control to root window with visual indentation
- **🎨 VCL Component Inspector** - Behavioral validation of Delphi/C++Builder components through Windows messages
- **📚 DLL/Library Inspector** - Complete enumeration with WOW64 bypass and heuristic Delphi version detection (XE2 to Delphi 13 Florence)
- **⏸️ Auto-pause** - Intelligent pause when mouse is over WinInspector for comfortable scrolling
- **Comprehensive details** - Handle, class name, title, dimensions, visibility, styles, process info
- **Multi-monitor support** - Resolution and work area details for complex setups
- **Tabbed interface** - Clean organization with Window Info and DLL/Libraries tabs
- **Professional UI** - No configuration needed, just run and inspect

🔧 Technical Details
- Built with Delphi and VCL
- Uses Windows API for low-level system access
- Lightweight and portable
- No installation required

🎯 Perfect for
- **Delphi/C++Builder developers** - Behavioral validation of VCL components, heuristic version detection, deployment troubleshooting
- **Windows developers** - UI debugging, window hierarchy analysis, cross-process inspection
- **System administrators** - DLL dependency analysis, troubleshooting compatibility issues
- **QA engineers** - Testing component states and window behavior in production
- **Anyone curious** about Windows internals and Delphi application architecture

## What's New in 2.1

Version 2.1 introduces major enhancements focused on Delphi/C++Builder developers:

- 🔗 **Complete Window Hierarchy** - Full parent chain visualization from any control to root window
- 🎨 **Behavioral Validation** - Verify VCL component identity through Windows message responses, not just class names
- 📚 **Heuristic Delphi Detection** - Automatic version identification from BPL package naming patterns
- ⏸️ **Auto-Pause** - Intelligent detection when mouse is over WinInspector for comfortable scrolling
- 🔧 **WOW64 Bypass** - Accurate DLL paths for 32-bit processes on 64-bit Windows

Read the [full release announcement](https://www.danieleteti.it/post/wininspector-release-2-1-en/) for technical details and use cases.

---

WinInspector is a modern evolution of the classic TDWinInfo tool with enhanced capabilities for today's multi-monitor, complex Windows environments and Delphi development workflows.
