import 'dart:io';
import 'package:flutter/material.dart';
import 'package:open_file/open_file.dart';

class LogFileBrowser extends StatefulWidget {
  final Directory? startDirectory;

  const LogFileBrowser({super.key, this.startDirectory});

  @override
  State<LogFileBrowser> createState() => _LogFileBrowserState();
}

class _LogFileBrowserState extends State<LogFileBrowser> {
  @override
  Widget build(BuildContext context) {
    Directory? currentDirectory = widget.startDirectory;

    if (currentDirectory == null) {
      return const Center(child: CircularProgressIndicator());
    }

    var files = currentDirectory.listSync();
    files.sort((a, b) => b.path.compareTo(a.path));

    return Scaffold(
      appBar: AppBar(
        title: Text(currentDirectory.path),
      ),
      body: ListView.builder(
        itemCount: files.length,
        itemBuilder: (context, index) {
          FileSystemEntity file = files[index];
          return ListTile(
            title: Text(file.path.split('/').last),
            onTap: () async {
              if (file is Directory) {
                Navigator.push(
                  context,
                  MaterialPageRoute(
                    builder: (context) => LogFileBrowser(startDirectory: file),
                  ),
                );
              } else {
                await OpenFile.open(file.path);
              }
            },
          );
        },
      ),
    );
  }
}
