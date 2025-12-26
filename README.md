# WinInspector 2.2 - Advanced Windows Analysis Tool

<p align="center">
  <img src="https://raw.githubusercontent.com/danieleteti/wininspector/main/docs/wininspector_logo.png" alt="WinInspector Logo" width="300">
</p>

WinInspector is a real-time Windows inspection utility that provides comprehensive information about windows, processes, and monitors under your mouse cursor. Born as the spiritual successor to TDWinInfo (originally developed 20+ years ago), WinInspector offers significantly enhanced functionality and modern capabilities.

📖 **Documentation**: [danieleteti.it/wininspector](https://www.danieleteti.it/wininspector)

![WinInspector Screenshot](https://raw.githubusercontent.com/danieleteti/wininspector/main/docs/wininspector_2_1.jpg)

🚀 Key Features
- **Real-time window inspection** - Live updates every 500ms as you move your mouse
- **Complete window hierarchy** - Full parent chain from control to root window with visual indentation
- **🎨 VCL Component Inspector** - Behavioral validation of Delphi/C++Builder components through Windows messages
- **📚 DLL/Library Inspector** - Complete enumeration with WOW64 bypass and heuristic Delphi version detection (XE2 to Delphi 13 Florence)
- **⏸️ Freeze/Pause** - Global hotkey (Ctrl+F12) to freeze inspection, auto-pause when mouse is over WinInspector
- **🎨 Color Picker** - Real-time pixel color under cursor (HEX and RGB)
- **📜 Inspection History** - Track last 50 inspected windows with timestamps
- **📋 Copy to Clipboard** - Quick export of window info and DLL lists
- **Comprehensive details** - Handle, class name, title, dimensions, visibility, styles, process info
- **Multi-monitor support** - Resolution and work area details for complex setups
- **Tabbed interface** - Window Info, DLL/Libraries, and History tabs
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

## What's New in 2.2

Version 2.2 focuses on usability improvements:

- ⏸️ **Global Hotkey Freeze** - Press Ctrl+F12 (or Alt+F12/Pause) anywhere to freeze current inspection
- 🎨 **Color Picker** - Real-time pixel color display in HEX (#RRGGBB) and RGB format
- 📜 **Inspection History** - New History tab tracks last 50 inspected windows with full details
- 📋 **Copy to Clipboard** - One-click copy of Window Info or DLL lists
- 🔢 **Version Display** - App version read from executable and shown in title bar

## What's New in 2.1

Version 2.1 introduced major enhancements for Delphi/C++Builder developers:

- 🔗 **Complete Window Hierarchy** - Full parent chain visualization from any control to root window
- 🎨 **Behavioral Validation** - Verify VCL component identity through Windows message responses
- 📚 **Heuristic Delphi Detection** - Automatic version identification from BPL package naming patterns
- ⏸️ **Auto-Pause** - Intelligent detection when mouse is over WinInspector
- 🔧 **WOW64 Bypass** - Accurate DLL paths for 32-bit processes on 64-bit Windows

📖 Read the [full 2.1 release announcement](https://www.danieleteti.it/post/wininspector-release-2-1-en/) for technical details and use cases.

---

WinInspector is a modern evolution of the classic TDWinInfo tool with enhanced capabilities for today's multi-monitor, complex Windows environments and Delphi development workflows.
