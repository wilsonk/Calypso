/**
 * D adaptation of the Qt5 Widgets first tutorial.
 *
 * Module map files from utils/modulemap/ should be installed in the libc and Qt include folders.
 *
 * Then build with:
 *   $ ldc2 -g -wi -v -cpp-args -D_REENTRANT -cpp-args -fPIE -cpp-args -DQT_WIDGETS_LIB -cpp-args -DQT_GUI_LIB -cpp-args -DQT_CORE_LIB -cpp-args -I/path/to/Qt/5.4/gcc_64/mkspecs/linux-g++ -cpp-args -I/path/to/Qt/5.4/gcc_64/include -cpp-args -I/path/to/Qt/5.4/gcc_64/include/QtWidgets -cpp-args -I/path/to/Qt/5.4/gcc_64/include/QtGui -cpp-args -I/path/to/Qt/5.4/gcc_64/include/QtCore -L-rpath=/path/to/Qt/5.4/gcc_64 -L-rpath=/path/to/Qt/5.4/gcc_64/lib -L-lQt5Widgets -L-lQt5Gui -L-lQt5Core -L-lGL -L-lstdc++ qt5demo.d
 */

modmap (C++) "<QtWidgets>";

import core.runtime;
import std.stdio, std.conv;

import (C++) Qt.QtCore;
import (C++) Qt.AlignmentFlag;

import (C++) QApplication, QString;
import (C++) QWidget, QTextEdit, QLineEdit, QLabel, QLayout, QGridLayout;

class AddressBook : QWidget
{
public:
    this(QWidget parent = null)
    {
        super(parent);

        auto nameStr = new QString("Name:");
        auto addressStr = new QString("Address:");
        auto titleStr = new QString("Simple Address Book");

        auto nameLabel = new QLabel(nameStr);
        nameLine = new QLineEdit;

        auto addressLabel = new QLabel(addressStr);
        addressText = new QTextEdit;

        auto mainLayout = new QGridLayout;
        mainLayout.addWidget(nameLabel, 0, 0); // explicit casts are needed, why?
        mainLayout.addWidget(nameLine, 0, 1);
        mainLayout.addWidget(addressLabel, 1, 0, new Alignment(AlignmentFlag.AlignTop));
        mainLayout.addWidget(addressText, 1, 1);

        setLayout(mainLayout);
        setWindowTitle(titleStr);
    }

private:
    QLineEdit nameLine;
    QTextEdit addressText;
}

int main()
{
    auto app = new QApplication(Runtime.cArgs.argc,
                Runtime.cArgs.argv);

    auto addressBook = new AddressBook;
    addressBook.show();

    return app.exec();
}
