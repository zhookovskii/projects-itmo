package hashslingingslasher.andoidhw.contacts

import android.view.View
import android.widget.Button
import androidx.recyclerview.widget.RecyclerView

class ContactViewHolder(root: View) : RecyclerView.ViewHolder(root) {
    val nameView: Button = root.findViewById(R.id.name)
    val phoneNumber: Button = root.findViewById(R.id.phoneNumber)

    fun bind(contact: Contact) {
        nameView.text = contact.name
        phoneNumber.text = contact.phoneNumber
    }
}