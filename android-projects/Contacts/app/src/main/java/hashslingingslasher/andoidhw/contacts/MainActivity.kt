package hashslingingslasher.andoidhw.contacts

import android.Manifest
import android.content.Intent
import android.content.pm.PackageManager
import android.net.Uri
import androidx.appcompat.app.AppCompatActivity
import android.os.Bundle
import android.util.Log
import android.widget.Button
import androidx.core.app.ActivityCompat
import androidx.core.content.ContextCompat
import androidx.recyclerview.widget.LinearLayoutManager
import androidx.recyclerview.widget.RecyclerView

class MainActivity : AppCompatActivity() {
    private var contactsPermissionGranted = false

    private lateinit var requestButton: Button

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        supportActionBar?.hide()
        if (ContextCompat.checkSelfPermission(this,
                Manifest.permission.READ_CONTACTS)
            != PackageManager.PERMISSION_GRANTED) {
            ActivityCompat.requestPermissions(this,
                arrayOf(Manifest.permission.READ_CONTACTS), READ_CONTACTS_PERMISSION)
            Log.i("PAPA MAKE ME HAPPY", "ASKED PERMISSION BRO")
        } else {
            contactsPermissionGranted = true
        }

        if (!contactsPermissionGranted) {
            setContentView(R.layout.no_permission_layout)
            requestButton = findViewById(R.id.requestPermissionButton)
            requestButton.setOnClickListener {
                recreate()
            }
        } else {
            setContentView(R.layout.activity_main)
            val viewManager = LinearLayoutManager(this)
            val recyclerView = findViewById<RecyclerView>(R.id.recyclerView)
            contactsList = if (contactsPermissionGranted) {
                fetchAllContacts().sortedBy { it.name.lowercase() }
            } else {
                emptyList()
            }
            recyclerView.apply {
                layoutManager = viewManager
                adapter = ContactAdapter(contactsList) {
                    val callIntent = Intent(Intent.ACTION_DIAL)
                    callIntent.data = Uri.parse("tel:" + it.phoneNumber)
                    startActivity(callIntent)
                }
            }
        }
    }

    override fun onSaveInstanceState(outState: Bundle) {
        outState.putBoolean(CONTACTS_PERMISSION_GRANTED, contactsPermissionGranted)
        super.onSaveInstanceState(outState)
    }

    override fun onRestoreInstanceState(savedInstanceState: Bundle) {
        super.onRestoreInstanceState(savedInstanceState)
        contactsPermissionGranted = savedInstanceState.getBoolean(CONTACTS_PERMISSION_GRANTED)
    }

    companion object {
        private const val READ_CONTACTS_PERMISSION = 100
        private const val CONTACTS_PERMISSION_GRANTED = "MainActivity.contactsPermissionGranted"
    }

    override fun onRequestPermissionsResult(
        requestCode: Int,
        permissions: Array<out String>,
        grantResults: IntArray
    ) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults)
        when (requestCode) {
            READ_CONTACTS_PERMISSION -> {
                contactsPermissionGranted = grantResults.isNotEmpty() && grantResults[0] ==
                        PackageManager.PERMISSION_GRANTED
                return
            }
        }
    }
}